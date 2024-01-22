#!/bin/bash -e

USAGE="[-m model-type] [--stdin] [--] QUESTION QUESTION QUESTION"

# Use flags or environment variables below:
MODEL_TYPE=${MODEL_TYPE:-mistral}
INPUT=${INPUT:-}
QUESTION=${QUESTION:-}
ERROR_OUTPUT="/dev/null"
TEMPERATURE=${TEMPERATURE:-0.33}
CONTEXT_LENGTH=${CONTEXT_LENGTH:=}
#MAX_CONTEXT_LENGTH=${CONTEXT_LENGTH}
N_PREDICT=""
SYSTEM_MESSAGE="${SYSTEM_MESSAGE-$(printf "%b" "Answer the following user question:\n")}"
SILENT_PROMPT="--silent-prompt"
NGL=""
PRIORITY="manual" # manual|speed|context
DEBUG=""
VERBOSE=${VERBOSE:-}
MODEL_RUNNER="/usr/bin/env"
DO_STDIN=""
THREADS=$(grep '^cpu cores\s*:' /proc/cpuinfo | head -1 | awk '{print $4}' || sysctl -n hw.ncpu || echo "$NUMBER_OF_PROCESSORS")

# memory allocation: assume 4 chars per token
PROMPT_LENGTH_EST=$(((75+${#SYSTEM_MESSAGE}+${#QUESTION}+${#INPUT})/4))
# echo "PROMPT_LENGTH_EST=$PROMPT_LENGTH_EST"

# If there are any args, require "--" or any non-hyphen word to terminate args and start question.
# Assume the whole args is a question if there is no hyphen to start.
if [[ "${1}" == "-"* ]]; then
    while [[ $# -gt 0 ]]; do
	case $1 in
	    -m)
		shift
		MODEL_TYPE="$1"
		;;
	    --speed)
		PRIORITY="speed"
		;;
	    --length)
		PRIORITY="length"
		;;
	    --temperature)
		shift
		TEMPERATURE="$1"
		;;
	    --verbose)
		VERBOSE=1
		;;
	    -c|--context-length)
		shift
		CONTEXT_LENGTH="$1"
		;;
	    --ngl)
		shift
		NGL="$1"
		;;
	    --n-predict)
		shift
		N_PREDICT="$1"
		;;
	    --debug)
		ERROR_OUTPUT="/dev/stdout"
		SILENT_PROMPT=""
		DEBUG=1
		;;
	    --stdin)
		DO_STDIN=1
		;;
	    --)
		shift
		QUESTION=("$@")
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


# Example usage:
# Set the variable to the path of the first existing file in the list.
# file_path=$(find_first_file /path/to/file1 /path/to/file2 /path/to/file3)
function find_first_file() {
  local files=("$@")
  local file
  for file in "${files[@]}"; do
    if [ -e "$file" ]; then
      echo "$file"
      return 0
    fi
  done
  return 1
}

function gpu_check {
    local layer_per_gb=("$@")
    if [ "${layer_per_gb}" == "" ]; then
        layer_per_gb=1
    fi
    if ! GPU=$(command -v nvidia-detector) || [[ "$GPU" == "None" ]]; then
        if [ "${DEBUG}" ]; then
            echo "* NO GPU"
        fi
        FREE_VRAM_GB=0
        NGL=0
        MAX_NGL_EST=0
    else
        # if gpu is already in use, estimate NGL max at int(free_vram_gb * 1.5)
        FREE_VRAM_GB=$(nvidia-smi --query-gpu=memory.free --format=csv,nounits,noheader | awk '{print $1 / 1024}')
        MAX_NGL_EST=$(awk -vfree_vram_gb=$FREE_VRAM_GB -vlayer_per_gb=$layer_per_gb "BEGIN{printf(\"%d\n\",int(free_vram_gb*layer_per_gb))}")
        if [ "${DEBUG}" ]; then
            echo "* FREE_VRAM_GB=${FREE_VRAM_GB} MAX_NGL_EST=${MAX_NGL_EST}"
        fi
    fi
}

function cap_ngl {
    if [ "${NGL}" -gt "${MAX_NGL_EST}" ]; then
        if [ "${DEBUG}" ]; then
            echo "* Capping $NGL at $MAX_NGL_EST"
        fi
        NGL=$MAX_NGL_EST
    fi
}

function dolphin_priority {
    case "${PRIORITY}" in
         speed)
            NGL=23
             CONTEXT_LENGTH=2048
             ;;
         context)
             NGL=8
             CONTEXT_LENGTH=12288
             ;;
         manual)
             NGL=${NGL:=23}
            CONTEXT_LENGTH=${CONTEXT_LENGTH:=2048}
             ;;
         *)
             echo "usage: unknown priority $PRIORITY"
             exit 1
            ;;
    esac
    cap_ngl
}

function mistral_priority {
    case "${PRIORITY}" in
        speed|context)
            NGL=33
            CONTEXT_LENGTH=2000
            ;;
        context)
            NGL=33
            CONTEXT_LENGTH=7999
            ;;
        manual)
            NGL=${NGL:=33}
            CONTEXT_LENGTH=${CONTEXT_LENGTH:=2000}
            ;;
        *)
            echo "usage: unknown priority $PRIORITY"
            exit 1
            ;;
    esac
    cap_ngl
}

function codebooga_priority {
    case "${PRIORITY}" in
         speed)
             NGL=33
             CONTEXT_LENGTH=2048
             ;;
         context)
             NGL=25
             CONTEXT_LENGTH=16383
             ;;
         manual)
            NGL=${NGL:=33}
            CONTEXT_LENGTH=${CONTEXT_LENGTH:=2048}
             ;;
        *)
             echo "Unknown -m ${MODEL_TYPE}"
             echo "usage: $0 ${USAGE}"
             exit 1
            ;;
    esac
    cap_ngl
}

function rocket_priority {
        MAX_CONTEXT_LENGTH=2048
        CONTEXT_LENGTH=${CONTEXT_LENGTH:=2048}
        BATCH_SIZE=${BATCH_SIZE:-128}
        NGL=${NGL:=0}
}

function phi_priority {
        MAX_CONTEXT_LENGTH=2048
        CONTEXT_LENGTH=${CONTEXT_LENGTH:=2048}
        BATCH_SIZE=${BATCH_SIZE:-128}
        NGL=${NGL:=0}
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
${INPUT}
${QUESTION}
Output:")
    fi
}

# Fit into estimated VRAM cap
case "${MODEL_TYPE}" in
    ## Model: dolphin mixtral 8x7b
    dolphin|mixtral)
        MODEL=${HOME}/wip/llamafiles/models/dolphin-2.5-mixtral-8x7b.Q4_K_M.llamafile
        MAX_CONTEXT_LENGTH=12288
        gpu_check 1.2
        chatml_prompt
        dolphin_priority
        ;;

    ## Model: mistral-7b-instruct
    mistral)
        MODEL=$(find_first_file \
                    ${HOME}/wip/llamafiles/models/mistral-7b-instruct-v0.2.Q4_K_M.llamafile \
                    ${HOME}/wip/llamafiles/models/mistral-7b-instruct-v0.1-Q4_K_M-main.llamafile \
                    ${HOME}/wip/llamafiles/models/mistral-7b-instruct-v0.2.Q5_K_M.llamafile \
                    ${HOME}/wip/llamafiles/models/mistral-7b-instruct-v0.2.Q3_K_S.llamafile)
        gpu_check 4
        MAX_CONTEXT_LENGTH=7999
        llama_prompt
        mistral_priority
        ;;
 
    ## Model: oobabooga/text-generation-webui/models/codebooga-34b-v0.1.Q4_K_M.gguf
    codebooga)
        MODEL_RUNNER="${HOME}/wip/llamafiles/bin/llamafile-main-0.1 -m "
        MODEL="${HOME}/wip/oobabooga/text-generation-webui/models/codebooga-34b-v0.1.Q4_K_M.gguf"
        MAX_CONTEXT_LENGTH=32768
        SILENT_PROMPT=""        # not supported by codebooga
        gpu_check 2.1
        llama_prompt
        codebooga_priority
        ;;

    rocket)
        MODEL="${HOME}/wip/llamafiles/models/rocket-3b.Q4_K_M.llamafile"
        # PROMPT=$(printf "%b" "<|im_start|>system\n{system_message}<|im_end|>\n<|im_start|>user\n{prompt}<|im_end|>\n<|im_start|>assistant\n")
        gpu_check 4
        chatml_prompt
        rocket_priority
        ;;

    phi)
        MODEL="${HOME}/wip/llamafiles/models/phi-2.Q6_K.llamafile"
        gpu_check 4
        phi_prompt
        phi_priority
        ;;

    *)
        echo "unknown model type $MODEL_TYPE"
        exit 1
        ;;
esac

if [ $(($PROMPT_LENGTH_EST * 2)) -gt "${CONTEXT_LENGTH}" ]; then
    echo "* WARNING: Prompt len $PROMPT_LENGTH_EST and response estimated not to fit in context ${CONTEXT_LENGTH}"
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

# Don't pass CLI args that aren't needed
N_PREDICT="${N_PREDICT:+--n-predict $N_PREDICT}"
NGL="${NGL:+-ngl $NGL}"
TEMPERATURE="${TEMPERATURE:+--temp $TEMPERATURE}"
CONTEXT_LENGTH="${CONTEXT_LENGTH:+-c $CONTEXT_LENGTH}"
BATCH_SIZE="${BATCH_SIZE:+--batch_size $BATCH_SIZE}"

# Set verbose and debug last
if [ "${DEBUG}" ] || [ "${VERBOSE}" ]; then
    printf '* ngl=%s context_length=%s est_len=%s:\n' "${NGL}" "${CONTEXT_LENGTH}" "${PROMPT_LENGTH_EST}"
    set -x
fi

printf '%s' "${PROMPT}" | ${MODEL_RUNNER} ${MODEL} ${TEMPERATURE} ${CONTEXT_LENGTH} ${NGL} ${N_PREDICT} ${BATCH_SIZE} --no-penalize-nl --repeat-penalty 1 -t ${THREADS} -f /dev/stdin $SILENT_PROMPT 2> "${ERROR_OUTPUT}"

# TODO: CLI parameters vs ENV vs bundles of settings is a mess
# Sort out --length/--ngl vs --speed/--length vs default
# sort out -c vs --length
# idea: calculate context length = MIN_CONTEXT_LENGTH <= (input length * 2) <= MAX_CONTEXT_LENGTH???
