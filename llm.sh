#!/bin/bash

USAGE="[-m|--model-type model-type] [--stdin|--interactive|-i] [--speed | --length] [--temperature temp] [--context-length|-c n] [--ngl n] [--n-predict n] [--debug] [--verbose] [--] QUESTION*"

# Use flags or environment variables below:
MODEL_TYPE=${MODEL_TYPE:-mistral}
INPUT=${INPUT:-}
QUESTION=${QUESTION:-}
ERROR_OUTPUT="/dev/null"
TEMPERATURE=${TEMPERATURE:-0.33}
CONTEXT_LENGTH=${CONTEXT_LENGTH:=}
N_PREDICT=""
SYSTEM_MESSAGE="${SYSTEM_MESSAGE-$(printf "%b" "Answer the following user question:\n")}"
SILENT_PROMPT="--silent-prompt"
NGL=""
GPU="--gpu auto"
PRIORITY="manual" # manual|speed|context
DEBUG=""
VERBOSE=${VERBOSE:-}
MODEL_RUNNER="/usr/bin/env"
DO_STDIN="$(test -t 0 || echo $?)"
LOG_DISABLE="--log-disable"

LLAMAFILE_MODEL_RUNNER="${HOME}/wip/llamafiles/bin/llamafile-0.6.1 -m "

# Get thread count
if [ "${THREADS}" == "" ]; then
    THREADS=$( ( [ -f /proc/cpuinfo ] && grep '^cpu cores\s*:' /proc/cpuinfo | head -1 | awk '{print $4}' ))
    if [ "${THREADS}" == "" ]; then
	THREADS=$(sysctl -n hw.ncpu 2>/dev/null || echo "${NUMBER_OF_PROCESSORS:-4}")
    fi
fi
THREADS="${THREADS:+-t $THREADS}"

# If there are any args, require "--" or any non-hyphen word to terminate args and start question.
# Assume the whole args is a question if there is no hyphen to start.
if [[ "${1}" == "-"* ]]; then
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
	    --verbose)
		VERBOSE=1 ;;
	    -c|--context-length)
		shift; CONTEXT_LENGTH="$1" ;;
	    --ngl)
		shift; NGL="$1" ;;
	    --n-predict)
		shift; N_PREDICT="$1" ;;
	    --debug)
		ERROR_OUTPUT="/dev/stdout"; SILENT_PROMPT=""; DEBUG=1 ;;
	    --stdin|--interactive|-i)
		DO_STDIN=1 ;;
	    --)
		# consumes rest of line
		shift; QUESTION=("$@")
		break
		;;
	    -*)
		echo "Unrecognized option: $1"
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


if [ "$DO_STDIN" != "" ]; then
    if [ -t 0 ]; then
        echo "Give input followed by Ctrl-D:"
    fi
    INPUT=$(cat)
fi

# memory allocation: assume 4 chars per token
PROMPT_LENGTH_EST=$(((75+${#SYSTEM_MESSAGE}+${#QUESTION}+${#INPUT})/4))
[ $VERBOSE ] && echo "* PROMPT_LENGTH_EST=$PROMPT_LENGTH_EST"

# Find the first existing executable in the list.
# file_path=$(find_first_file /path/to/file1 /path/to/file2 /path/to/file3)
function find_first_file() {
  local files=("$@")
  local file
  for file in "${files[@]}"; do
    if [ -x "$file" ]; then
      [ $VERBOSE ] && echo "* Model $file" >> /dev/stderr
      echo "${file}"
      return 0
    fi
  done
  echo "* Cannot find executable model in $@" >> /dev/stderr
  return 1
}

function gpu_check {
    local layer_per_gb=("$@")
    if [ "${layer_per_gb}" == "" ]; then
        layer_per_gb=1
    fi
    if ! gpu_detector=$(command -v nvidia-detector) || [[ "$($gpu_detector)" == "None" ]]; then
        if [ "${DEBUG}" ]; then
            echo "* NO GPU"
        fi
        FREE_VRAM_GB=0
        MAX_NGL_EST=0
        NGL=""
	GPU="--gpu none"
    else
        # if gpu is already in use, estimate NGL max at int(free_vram_gb * 1.5)
        FREE_VRAM_GB=$(nvidia-smi --query-gpu=memory.free --format=csv,nounits,noheader | awk '{print $1 / 1024}')
	if (( $(echo "${FREE_VRAM_GB} < 2" |bc -l) )); then
	    GPU="--gpu none"
	    NGL_MAX_EST=0
	    NGL=""
	else
	    GPU="--gpu nvidia"
            MAX_NGL_EST=$(awk -vfree_vram_gb=$FREE_VRAM_GB -vlayer_per_gb=$layer_per_gb "BEGIN{printf(\"%d\n\",int(free_vram_gb*layer_per_gb))}")
	fi
    fi
    if [ "${DEBUG}" ]; then
	echo "* FREE_VRAM_GB=${FREE_VRAM_GB} MAX_NGL_EST=${MAX_NGL_EST} GPU=${GPU}"
    fi
}

function cap_ngl {
    if [ "$GPU" == "" ]; then
	GPU="--gpu none"
    else
	NGL=$MAX_NGL_EST
	if [ "${NGL}" != '' ] && [ "${NGL}" -gt "${MAX_NGL_EST}" ]; then
            [ $VERBOSE ] && echo "* Capping $NGL at $MAX_NGL_EST"
            NGL=$MAX_NGL_EST
	fi
    fi
}

function dolphin_priority {
    MAX_CONTEXT_LENGTH=12288
    case "${PRIORITY}" in
         speed)
             NGL=${NGL:=23}
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
             echo "usage: unknown priority $PRIORITY"
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
            echo "usage: unknown priority $PRIORITY"
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
             echo "usage: unknown priority $PRIORITY"
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
             echo "usage: unknown priority $PRIORITY"
             echo "usage: $0 ${USAGE}"
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
             echo "usage: unknown priority $PRIORITY"
             echo "usage: $0 ${USAGE}"
             exit 1
            ;;
    esac

    cap_ngl
}

function phi_priority {
    MAX_CONTEXT_LENGTH=2048
    CONTEXT_LENGTH=${CONTEXT_LENGTH:=2048}
    BATCH_SIZE=${BATCH_SIZE:-128}
    NGL=${NGL:-}
    cap_ngl
}


function llama_prompt {
    PROMPT=$(printf "%b" "[INST]${SYSTEM_MESSAGE}\n${QUESTION}\n${INPUT}[/INST]\n")
}

function chatml_prompt {
    PROMPT=$(printf "%b" "<|im_start|>system
${SYSTEM_MESSAGE}<|im_end|>
<|im_start|>user
${QUESTION}
${INPUT}<|im_end|>
<|im_start|>assistant
")
}


function phi_prompt {
    # Instruct: {prompt}
    # Output:
    if [ "${INPUT}" == "" ]; then
      PROMPT=$(printf "%b" "Instruct: ${SYSTEM_MESSAGE}
${QUESTION}
Output:")
    else
      PROMPT=$(printf "%b" "Instruct: ${QUESTION}
User Input:
-----------------
${INPUT}
-----------------
End of User Input
Output:")
    fi
}

# Fit into estimated VRAM cap
case "${MODEL_TYPE}" in
    ## Model: dolphin mixtral 8x7b
    ## todo: some are not dolphin, only mixtral
    dolphin|mixtral)
        MODEL=$(find_first_file \
		${HOME}/wip/llamafiles/models/dolphin-2.7-mixtral-8x7b.Q4_K_M.gguf \
		${HOME}/wip/llamafiles/models/mixtral-8x7b-instruct-v0.1.Q5_K_M.llamafile \
	        ${HOME}/wip/llamafiles/models/mixtral_7bx2_moe.Q3_K_M.gguf \
		)
        gpu_check 1
        chatml_prompt
        dolphin_priority
        ;;

    ## Model: mistral-7b-instruct
    mistral)
        MODEL=$(find_first_file \
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
    codebooga)
        MODEL="${HOME}/wip/oobabooga/text-generation-webui/models/codebooga-34b-v0.1.Q4_K_M.gguf"
        SILENT_PROMPT=""        # not supported by codebooga
        gpu_check 2.1
        llama_prompt
        codebooga_priority
        ;;

    ## Model: deepseek-coder
    deepseek|coder)
        MODEL="${HOME}/wip/llamafiles/models/deepseek-coder-6.7b-instruct.Q4_K_M.gguf"
        SILENT_PROMPT=""
        gpu_check 2.1
	llama_prompt
        deepseek_priority
        ;;

    rocket)
        MODEL=$(find_first_file \
		    "${HOME}/wip/llamafiles/models/rocket-3b.Q6_K.llamafile" \
		    "${HOME}/wip/llamafiles/models/rocket-3b.Q5_K_M.llamafile" \
		    "${HOME}/wip/llamafiles/models/rocket-3b.Q4_K_M.llamafile" \
	     )
        gpu_check 4
        chatml_prompt
        rocket_priority
        ;;

    phi)
        MODEL=$(find_first_file \
		    "${HOME}/wip/llamafiles/models/phi-2.Q6_K.llamafile" \
		    "${HOME}/wip/llamafiles/models/phi-2.Q5_K_M.llamafile" \
	     )
        gpu_check 4
        phi_prompt
        phi_priority
        ;;

    *)
        echo "unknown model type $MODEL_TYPE"
        exit 1
        ;;
esac

if [ "${PROMPT_LENGTH_EST}" -gt "${CONTEXT_LENGTH}" ]; then
    echo "* WARNING: Prompt len ${PROMPT_LENGTH_EST} estimated not to fit in context ${CONTEXT_LENGTH}"
fi

if [ "$CONTEXT_LENGTH" -gt "$MAX_CONTEXT_LENGTH" ]; then
    CONTEXT_LENGTH="$MAX_CONTEXT_LENGTH"
    echo "* Truncated context length to $CONTEXT_LENGTH"
fi

if [ ! -f $MODEL ]; then
    echo "Model not found: ${MODEL}"
    exit 1
fi

PROMPT_LENGTH_EST=$((${#PROMPT}/4))
#BATCH_SIZE=${BATCH_SIZE:-$(($CONTEXT_LENGTH / 2))}

# If no GPU, force NGL off
if [ "${GPU}" = '' ]; then
    NGL=0
    GPU="--gpu none"
fi

# Don't pass CLI args that aren't needed
N_PREDICT="${N_PREDICT:+--n-predict $N_PREDICT}"
NGL="${NGL:+-ngl $NGL}"
TEMPERATURE="${TEMPERATURE:+--temp $TEMPERATURE}"
CONTEXT_LENGTH="${CONTEXT_LENGTH:+-c $CONTEXT_LENGTH}"
BATCH_SIZE="${BATCH_SIZE:+--batch_size $BATCH_SIZE}"

# set MODEL_RUNNER
if [ "${MODEL##*.}" != "llamafile" ]; then
   MODEL_RUNNER="${LLAMAFILE_MODEL_RUNNER}"
fi

# Set verbose and debug last
if [ "${DEBUG}" ] || [ "${VERBOSE}" ]; then
    printf '* Paramters: ngl=%s context_length=%s est_len=%s:\n' "${NGL}" "${CONTEXT_LENGTH}" "${PROMPT_LENGTH_EST}"
    set -x
fi

printf '%s' "${PROMPT}" | ${MODEL_RUNNER} ${MODEL} ${LOG_DISABLE} ${TEMPERATURE} ${CONTEXT_LENGTH} ${NGL} ${N_PREDICT} ${BATCH_SIZE} --no-penalize-nl --repeat-penalty 1 ${THREADS} -f /dev/stdin $SILENT_PROMPT 2> "${ERROR_OUTPUT}"

# TODO: bash parsing of CLI parameters vs ENV vs bundles of settings is a mess
# Sort out --length/--ngl vs --speed/--length vs default
# sort out -c vs --length
# idea: calculate context length = MIN_CONTEXT_LENGTH <= (input length * 2) <= MAX_CONTEXT_LENGTH???
