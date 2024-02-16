#!/bin/bash

USAGE="[-m|--model-type model-type] [--stdin|--interactive|-i] [--speed | --length] [--temperature temp] [--context-length|-c n] [--ngl n] [--n-predict n] [--debug] [--verbose|-v] [--] QUESTION*"

# Use CLI flags, or environment variables below:
MODEL_TYPE=${MODEL_TYPE:-mistral}
ERROR_OUTPUT="/dev/null"
TEMPERATURE=${TEMPERATURE:-}
CONTEXT_LENGTH=${CONTEXT_LENGTH:-}
N_PREDICT="${N_PREDICT:-}"
SYSTEM_MESSAGE="${SYSTEM_MESSAGE-"Answer the following user question:"}"
SILENT_PROMPT="--silent-prompt"
NGL="${NGL:-}"
GPU="--gpu auto"
PRIORITY="${PRIORITY:-manual}" # speed|length|manual
DEBUG="${DEBUG:-}"
VERBOSE=${VERBOSE:-}
LOG_DISABLE="--log-disable"
GRAMMAR_FILE="${GRAMMAR_FILE:-}"
BATCH_SIZE="${BATCH_SIZE:-}"

# Not settable via ENV
MODEL_RUNNER="/usr/bin/env"
PROCESS_QUESTION_ESCAPES=""
LLAMAFILE_MODEL_RUNNER="${HOME}/wip/llamafiles/lib/llamafile-0.6.2 -m "
CLI_MODE="--cli"

# Read input
INPUT=""
QUESTION=""
DO_STDIN="$(test -t 0 || echo $?)"

# Get thread count
if [ "${THREADS}" == "" ];
then
    THREADS=$( ( [ -f /proc/cpuinfo ] && grep '^cpu cores\s*:' /proc/cpuinfo | head -1 | awk '{print $4}' ))
    if [ "${THREADS}" == "" ];
    then
        THREADS=$(sysctl -n hw.ncpu 2>/dev/null || echo "${NUMBER_OF_PROCESSORS:-4}")
    fi
fi
THREADS="${THREADS:+-t $THREADS}"

# If there are any args, require "--" or any non-hyphen word to terminate args and start question.
# Assume the whole args is a question if there is no hyphen to start.
if [[ "${1}" == "-"* ]];
then
    while [[ $# -gt 0 ]]; do
        case $1 in
            -m|--model-type)
                shift; MODEL_TYPE="$1" ;;
            --speed)
                PRIORITY="speed" ;;
            --length)
                PRIORITY="length" ;;
            --temperature)
                shift; TEMPERATURE="$1" ;;
            --verbose|-v)
                VERBOSE=1 ;;
            -c|--context-length)
                shift; CONTEXT_LENGTH="$1" ;;
            --ngl)
                shift; NGL="$1" ;;
            --n-predict)
                shift; N_PREDICT="$1" ;;
            --grammar-file)
                shift; GRAMMAR_FILE="--grammar-file $1" ;;
            --debug)
                ERROR_OUTPUT="/dev/stdout"; SILENT_PROMPT=""; DEBUG=1 ;;
            --noerror)
                ERROR_OUTPUT="/dev/null" ;;
            --stdin|--interactive|-i)
                DO_STDIN=1 ;;
	    -e|--process-question-escapes)
		PROCESS_QUESTION_ESCAPES=1 ;;
            --)
                # consumes rest of line
                shift; QUESTION=("$@")
                break
                ;;
            -*)
                echo "Unrecognized option: $1" >> /dev/stderr
                exit 1
                ;;
            *)
                # consumes rest of line
                QUESTION=("$*")
                break
                ;;
        esac
        shift
    done
else
    QUESTION="${*}"
fi

# Process escape sequences in QUESTION if requested.
# STDIN never processes escapes.
if [ "${PROCESS_QUESTION_ESCAPES}" ]; then
    [ $VERBOSE ] && echo "* Processing escape sequences in QUESTION"
    printf -v QUESTION "%b" "$QUESTION"
fi

if [ "$DO_STDIN" != "" ];
then
    if [ -t 0 ];
    then
        echo "Give input followed by Ctrl-D:"
    fi
    INPUT=$(cat)
fi

# memory allocation: assume 4 chars per token
PROMPT_LENGTH_EST=$(((75+${#SYSTEM_MESSAGE}+${#QUESTION}+${#INPUT})/4))
[ $VERBOSE ] && echo "* PROMPT_LENGTH_EST=$PROMPT_LENGTH_EST"

# Find the first existing executable or GGUF in the list.
# file_path=$(find_first_model /path/to/file1 /path/to/file2 /path/to/file3)
function find_first_model() {
  local files=("$@")
  local file
  for file in "${files[@]}"; do
    [ $VERBOSE ] && echo "* Checking Model $file" >> /dev/stderr
    if [ -x "$file" ] || [ "${file##*.}" == "gguf" ];
    then
      [ $VERBOSE ] && echo "* Accepting Model $file" >> /dev/stderr
      echo "${file}"
      return 0
    fi
  done
  echo "* Cannot find executable model in $@" >> /dev/stderr
  return 1
}

function gpu_check {
    local layer_per_gb=("$@")
    if [ "${layer_per_gb}" == "" ];
    then
        layer_per_gb=1
    fi
    if ! gpu_detector=$(command -v nvidia-detector) || [[ "$($gpu_detector)" == "None" ]];
    then
        if [ "${DEBUG}" ];
        then
            echo "* NO GPU"
        fi
        FREE_VRAM_GB=0
        MAX_NGL_EST=0
        NGL=""
        GPU="--gpu none"
    else
        # if gpu is already in use, estimate NGL max at int(free_vram_gb * 1.5)
        FREE_VRAM_GB=$(nvidia-smi --query-gpu=memory.free --format=csv,nounits,noheader | awk '{print $1 / 1024}')
        if (( $(echo "${FREE_VRAM_GB} < 2" |bc -l) ));
        then
            GPU="--gpu none"
            MAX_NGL_EST=0
            NGL=0
        else
            GPU="--gpu nvidia"
            MAX_NGL_EST=$(awk -vfree_vram_gb=$FREE_VRAM_GB -vlayer_per_gb=$layer_per_gb "BEGIN{printf(\"%d\n\",int(free_vram_gb*layer_per_gb))}")
        fi
    fi
    if [ "${DEBUG}" ];
    then
        echo "* FREE_VRAM_GB=${FREE_VRAM_GB} MAX_NGL_EST=${MAX_NGL_EST} GPU=${GPU}"
    fi
}

function cap_ngl {
    if [ "$GPU" == "" ];
    then
        GPU="--gpu none"
    else
        if [ "${NGL}" != "" ] && [ "${NGL}" -gt "${MAX_NGL_EST}" ];
        then
            [ $VERBOSE ] && echo "* Capping $NGL at $MAX_NGL_EST"
            NGL=$MAX_NGL_EST
        fi
    fi
}

function mixtral_priority {
    MAX_CONTEXT_LENGTH=12288
    case "${PRIORITY}" in
         speed)
             NGL=${NGL:=23}
             CONTEXT_LENGTH=2048
             ;;
         length)
             NGL=${NGL:=1}
             CONTEXT_LENGTH=12288
             ;;
         manual)
             NGL=${NGL:=23}
             CONTEXT_LENGTH=${CONTEXT_LENGTH:=4096}
             ;;
         *)
             echo "usage: unknown priority $PRIORITY" >> /dev/stderr
             exit 1
            ;;
    esac
    cap_ngl
}

function dolphin_priority {
    MAX_CONTEXT_LENGTH=12288
    case "${PRIORITY}" in
         speed)
             NGL=${NGL:=28}
             CONTEXT_LENGTH=2048
             ;;
         length)
             NGL=${NGL:=8}
             CONTEXT_LENGTH=12288
             ;;
         manual)
             NGL=${NGL:=23}
             CONTEXT_LENGTH=${CONTEXT_LENGTH:=4096}
             ;;
         *)
             echo "usage: unknown priority $PRIORITY" >> /dev/stderr
             exit 1
            ;;
    esac
    cap_ngl
}

function mistral_priority {
    MAX_CONTEXT_LENGTH=7999
    case "${PRIORITY}" in
        speed)
            NGL=${NGL:=33}
            CONTEXT_LENGTH=2048
            ;;
        length)
            NGL=${NGL:=33}
            CONTEXT_LENGTH=7999
            ;;
        manual)
            NGL=${NGL:=33}
            CONTEXT_LENGTH=${CONTEXT_LENGTH:=4096}
            ;;
        *)
            echo "usage: unknown priority $PRIORITY" >> /dev/stderr
            exit 1
            ;;
    esac

    cap_ngl
}

function codebooga_priority {
    MAX_CONTEXT_LENGTH=32768
    case "${PRIORITY}" in
         speed)
             NGL=${NGL:=33}
             CONTEXT_LENGTH=2048
             ;;
         length)
             NGL=${NGL:=25}
             CONTEXT_LENGTH=16383
             ;;
         manual)
            NGL=${NGL:=33}
            CONTEXT_LENGTH=${CONTEXT_LENGTH:=2048}
             ;;
        *)
             echo "usage: unknown priority $PRIORITY" >> /dev/stderr
             exit 1
            ;;
    esac

    cap_ngl
}

function deepseek_priority {
    MAX_CONTEXT_LENGTH=32768
    case "${PRIORITY}" in
         speed)
             NGL=${NGL:=33}
             CONTEXT_LENGTH=2048
             ;;
         length)
             NGL=${NGL:=25}
             CONTEXT_LENGTH=16383
             ;;
         manual)
            NGL=${NGL:=33}
            CONTEXT_LENGTH=${CONTEXT_LENGTH:=2048}
             ;;
        *)
             echo "usage: unknown priority $PRIORITY" >> /dev/stderr
             echo "usage: $0 ${USAGE}" >> /dev/stderr
             exit 1
            ;;
    esac

    cap_ngl
}

function rocket_priority {
    MAX_CONTEXT_LENGTH=4096
    case "${PRIORITY}" in
         speed)
             NGL=${NGL:=33}
             CONTEXT_LENGTH=2048
             ;;
         length)
             NGL=${NGL:=25}
             CONTEXT_LENGTH=4096
             ;;
         manual)
            NGL=${NGL:=33}
            CONTEXT_LENGTH=${CONTEXT_LENGTH:=2048}
             ;;
        *)
             echo "usage: unknown priority $PRIORITY" >> /dev/stderr
             echo "usage: $0 ${USAGE}" >> /dev/stderr
             exit 1
            ;;
    esac

    cap_ngl
}

function phi_priority {
    MAX_CONTEXT_LENGTH=2048
    CONTEXT_LENGTH=${CONTEXT_LENGTH:=2048}
    BATCH_SIZE=${BATCH_SIZE:=128}
    NGL=${NGL:-33}
    cap_ngl
}

function llama_prompt {
    if [ "${INPUT}" == "" ]; then
	printf -v PROMPT "[INST]
%s
%s
[/INST]
" "${SYSTEM_MESSAGE%$'\n'}" "${QUESTION%$'\n'}"
    else
	printf -v PROMPT "[INST]
%s
%s
%s
[/INST]
" "${SYSTEM_MESSAGE%$'\n'}" "${QUESTION%$'\n'}" "${INPUT%$'\n'}"
    fi
}

function alpaca_prompt {

    alpaca_header="Below is an instruction that describes a task, paired with an input that provides further context. Write a response that appropriately completes the request."
    if [ "${INPUT}" == "" ]; then
	##### NO INPUT CASE
	printf -v PROMPT "%s" "${alpaca_header}

### Instruction:
${SYSTEM_MESSAGE%$'\n'}

### Input:
${QUESTION%$'\n'}

### Response:

"
	##### END NO INPUT CASE
    else
	##### INPUT CASE
	printf -v PROMPT "
%s
### Instruction:
%s

%s

### Input:
%s

### Response:

" "${alpaca_header}" "${SYSTEM_MESSAGE%$'\n'}" "${QUESTION%$'\n'}" "${INPUT%$'\n'}"
	##### END NO INPUT CASE
    fi
}

function chatml_prompt {
    if [ "${INPUT}" == "" ]; then
        printf -v PROMPT "<|im_start|>system
%s
<|im_end|>
<|im_start|>user
%s<|im_end|>
<|im_start|>assistant" "${SYSTEM_MESSAGE%$'\n'}" "${QUESTION%$'\n'}"
    else
        printf -v PROMPT "<|im_start|>system
%s<|im_end|>
<|im_start|>user
%s
%s<|im_end|>
<|im_start|>assistant" "${SYSTEM_MESSAGE%$'\n'}" "${QUESTION%$'\n'}" "${INPUT%$'\n'}"
    fi
}

function phi_prompt {
    # Instruct: {prompt}
    # Output:
    if [ "${INPUT}" == "" ];
    then
      printf -v PROMPT "%s" "Instruct: ${SYSTEM_MESSAGE%$'\n'}
%s
Output:" "${QUESTION}"
    else
      printf -v PROMPT "%s" "Instruct: %s
User Input:
-----------------
%s
-----------------
End of User Input
Output:" "${QUESTION%$'\n'}" "${INPUT}"
    fi
}

# Fit into estimated VRAM cap
case "${MODEL_TYPE}" in
    # Mixtral
    mixtral)
        MODEL=$(find_first_model \
                ${HOME}/wip/llamafiles/models/mixtral-8x7b-instruct-v0.1.Q5_K_M.llamafile \
                )
        gpu_check 1
        chatml_prompt
        mixtral_priority
        ;;

    # Dolphin of various sorts
    dolphin)
        MODEL=$(find_first_model \
                ${HOME}/wip/llamafiles/models/dolphin-2.7-mixtral-8x7b.Q4_K_M.gguf \
                )
        gpu_check 1.3
        chatml_prompt
        dolphin_priority
        ;;

    ## Model: mistral-7b-instruct
    mistral)
        MODEL=$(find_first_model \
                    ${HOME}/wip/llamafiles/models/mistral-7b-instruct-v0.2.Q5_K_M.llamafile \
                    ${HOME}/wip/llamafiles/models/mistral-instruct-v0.2.Q5_K_M.llamafile \
                    ${HOME}/wip/llamafiles/models/mistral-7b-instruct-v0.2.Q4_K_M.llamafile \
                    ${HOME}/wip/llamafiles/models/mistral-7b-instruct-v0.1-Q4_K_M-main.llamafile \
                    ${HOME}/wip/llamafiles/models/mistral-7b-instruct-v0.2.Q3_K_M.llamafile \
                    ${HOME}/wip/llamafiles/models/mistral-7b-instruct-v0.2.Q3_K_S.llamafile)
        gpu_check 4
        llama_prompt
        mistral_priority
        ;;
 
    ## Model: oobabooga/text-generation-webui/models/codebooga-34b-v0.1.Q4_K_M.gguf
    ## Model: {$HOME}/wip/llamafiles/models/deepseek-coder-6.7b-instruct.Q4_K_M.gguf
    codebooga)
        MODEL=$(find_first_model \
                    "${HOME}/wip/oobabooga/text-generation-webui/models/codebooga-34b-v0.1.Q4_K_M.gguf" \
                )
        SILENT_PROMPT=""        # not supported by codebooga
        gpu_check 2.1
        alpaca_prompt
        codebooga_priority
        ;;

    ## Model: deepseek-coder
    deepseek|coder)
        MODEL=$(find_first_model \
                    "${HOME}/wip/llamafiles/models/deepseek-coder-6.7b-instruct.Q4_K_M.gguf" \
               )
        SILENT_PROMPT=""
        gpu_check 2.1
        llama_prompt
        deepseek_priority
        ;;

    rocket)
        MODEL=$(find_first_model \
                    "${HOME}/wip/llamafiles/models/rocket-3b.Q6_K.llamafile" \
                    "${HOME}/wip/llamafiles/models/rocket-3b.Q5_K_M.llamafile" \
                    "${HOME}/wip/llamafiles/models/rocket-3b.Q4_K_M.llamafile" \
             )
        gpu_check 4
        chatml_prompt
        rocket_priority
        ;;

    phi)
        MODEL=$(find_first_model \
                    "${HOME}/wip/llamafiles/models/phi-2.Q6_K.llamafile" \
                    "${HOME}/wip/llamafiles/models/phi-2.Q5_K_M.llamafile" \
             )
        gpu_check 4
        phi_prompt
        phi_priority
        ;;

    *)
        echo "unknown model type $MODEL_TYPE" >> /dev/stderr
        exit 1
        ;;
esac

if [ "${PROMPT_LENGTH_EST}" -gt "${CONTEXT_LENGTH}" ];
then
    echo "* WARNING: Prompt len ${PROMPT_LENGTH_EST} estimated not to fit in context ${CONTEXT_LENGTH}"
fi

if [ "$CONTEXT_LENGTH" -gt "$MAX_CONTEXT_LENGTH" ];
then
    CONTEXT_LENGTH="$MAX_CONTEXT_LENGTH"
    echo "* Truncated context length to $CONTEXT_LENGTH"
fi

if [ ! -f $MODEL ];
then
    echo "Model not found: ${MODEL}" >> /dev/stderr
    exit 2
fi

PROMPT_LENGTH_EST=$((${#PROMPT}/4))
#BATCH_SIZE=${BATCH_SIZE:-$(($CONTEXT_LENGTH / 2))}

# If no GPU, force NGL off
if [ "${GPU}" = "" ];
then
    NGL=0
    GPU="--gpu none"
fi

# Don't pass CLI args that aren't needed
N_PREDICT="${N_PREDICT:+--n-predict $N_PREDICT}"
NGL="${NGL:+-ngl $NGL}"
TEMPERATURE="${TEMPERATURE:+--temp $TEMPERATURE}"
CONTEXT_LENGTH="${CONTEXT_LENGTH:+-c $CONTEXT_LENGTH}"
BATCH_SIZE="${BATCH_SIZE:+--batch_size $BATCH_SIZE}"

if [ "${MODEL}" == "" ];
then
    echo "* FAIL: No model" >> /dev/stderr
    exit 2
fi

# set MODEL_RUNNER
if [ "${MODEL##*.}" != "llamafile" ];
then
   MODEL_RUNNER="${LLAMAFILE_MODEL_RUNNER}"
fi

# Set verbose and debug last
if [ "${DEBUG}" ] || [ "${VERBOSE}" ];
then
    printf '* Parameters: ngl=%s context_length=%s est_len=%s:\n' "${NGL}" "${CONTEXT_LENGTH}" "${PROMPT_LENGTH_EST}"
    set -x
fi

# Perform inference
#set -x
printf '%s' "${PROMPT}" | ${MODEL_RUNNER} ${MODEL} ${CLI_MODE} ${LOG_DISABLE} ${GRAMMAR_FILE} ${TEMPERATURE} ${CONTEXT_LENGTH} ${NGL} ${N_PREDICT} ${BATCH_SIZE} --no-penalize-nl --repeat-penalty 1 ${THREADS} -f /dev/stdin $SILENT_PROMPT 2> "${ERROR_OUTPUT}"
STATUS=$?

# Try to inform user about errors
if [ "$STATUS" != "0" ];
then
    if [ "${ERROR_OUTPUT}" == "/dev/null" ];
    then
	echo "* FAIL $STATUS: re-run with --debug" > /dev/stderr
    else
	echo "* FAIL $STATUS: errors went to ${ERROR_OUTPUT}" > /dev/stderr
    fi
fi

exit $STATUS


# TODO: add few-shot to supplement system message, since at least in chatml each goes in as an assistant turn
# TODO: bash parsing of CLI parameters vs ENV vs bundles of settings is a mess
# CAMERA MODE:
# by analogy to aperture priority, shutter priority, auto, or manual
# context priority, speed priority, auto, or manual
# CONTEXT PRIORITY MODE:
# - could be used for input or output
#   for input, assume some k * (prompt len + input len)
#   for output, probably need an explicit declaration we want a long output, hard to detect otherwise
# SPEED PRIORITY MODE:
# - `--speed` `--ngl`
# AUTO MODE:
# - proposal: `--speed` unless cmd input is given, then do `-context`
#   e.g. MIN_CONTEXT_LENGTH <= (input length * 2) <= MAX_CONTEXT_LENGTH???
# MANUAL MODE:
# - `--ngl` `--context-length`

# sort out --length/--ngl vs --speed/--length vs default
# re-examine cap_ngl and *_priority 

# sort out -c vs --length

# 
