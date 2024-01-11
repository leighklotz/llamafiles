#!/bin/bash

USAGE="[-m model-type] [--stdin] [--] QUESTION QUESTION QUESTION"

# Use flags or environment variables below:
MODEL_TYPE=${MODEL_TYPE:-mistral}
INPUT=${INPUT:-}
QUESTION=${QUESTION:-}
ERROR_OUTPUT="/dev/null"
TEMPERATURE=${TEMPERATURE:-0.33}
CONTEXT_LENGTH=${CONTEXT_LENGTH:-7999}
MAX_CONTEXT_LENGTH=${CONTEXT_LENGTH}
SYSTEM_MESSAGE="${SYSTEM_MESSAGE-$(printf "%b" "Answer the following user question:\n")}"
SILENT_PROMPT="--silent-prompt"
BATCH_SIZE=${BATCH_SIZE:-$(($CONTEXT_LENGTH / 2))}
NGL=""
PRIORITY="manual" # manual|speed|context
DEBUG=""

# memory allocation: assume 4 chars per token
PROMPT_LENGTH_EST=$(((75+${#SYSTEM_MESSAGE}+${#QUESTION}+${#INPUT})/4))

# TODO: CLI parameters vs ENV vs bundles of settings is a mess
# Sort out --length/--ngl vs --speed/--length vs default
# sort out -c vs --length
# idea: calculate context length = MIN_CONTEXT_LENGTH <= (input length * 2) <= MAX_CONTEXT_LENGTH???

# echo "PROMPT_LENGTH_EST=$PROMPT_LENGTH_EST"

# If there are any args, require "--" or any non-hyphen word to terminate args and start question.
# Assume the whole args is a question if there is no hyphen to start.
if [[ "${1}" == "-"* ]]; then
    for ((index=1; index<$#; index ++)); do
	arg=${@:$index:1}
	if [[ "${arg}" == "-m" ]]; then
	    ((index ++));
	    MODEL_TYPE="${@:$index:1}"
	elif [[ "${arg}" == "--speed" ]]; then
	    PRIORITY="speed"
	elif [[ "${arg}" == "--length" ]]; then
	    PRIORITY="length"
	elif [[ "${arg}" == "-c" ]]; then
	    ((index ++));
	    CONTEXT_LENGTH="${@:$index:1}"
	elif [[ "${arg}" != "-"* ]]; then
	    QUESTION=("${*:index + 1}")
	    break
	elif [[ "${arg}" == "--ngl" ]]; then
	    ((index ++));
	    NGL="${@:$index:1}"
	elif [[ "${arg}" == "--debug" ]]; then
	  ERROR_OUTPUT="/dev/stdout"
	  SILENT_PROMPT=""
	  DEBUG=1
	elif [[ "${arg}" == "--stdin" ]]; then
	    if [ -t 0 ]; then
		echo "Give input followed by Ctrl-D:"
	    fi
	    INPUT=$(cat)
	elif [[ "${arg}" == "--" ]]; then
	    QUESTION=("${*:index + 1}")
	    break
	fi
    done
else
    QUESTION="${*}"
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

function dolphin_priority {
    case "${PRIORITY}" in
 	speed)
	    NGL=33
 	    CONTEXT_LENGTH=2048
 	    ;;
 	context)
 	    NGL=8
 	    CONTEXT_LENGTH=12288
 	    ;;
 	manual)
	    if [[ -z "${NGL:-}" ]]; then
 		NGL=${NGL:=20}
	        CONTEXT_LENGTH=${CONTEXT_LENGTH:=4000}
	    fi
 	    ;;
 	*)
 	    echo "usage: unknown priority $PRIORITY"
 	    exit 1
	    ;;
    esac
}

function mixtral_priority {
    case "${PRIORITY}" in
	speed)
	    NGL=23
	    CONTEXT_LENGTH=2048
	    ;;
	context)
	    NGL=8
	    CONTEXT_LENGTH=7999
	    ;;
	manual)
	    if [[ -z "${NGL:-}" ]]; then
 		NGL=${NGL:=33}
	        CONTEXT_LENGTH=${CONTEXT_LENGTH:=4000}
	    fi
	    ;;
	*)
	    echo "usage: unknown priority $PRIORITY"
	    exit 1
	    ;;
    esac
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
	    if [[ -z "${NGL:-}" ]]; then
 		NGL=33
		CONTEXT_LENGTH=2048
	    fi
 	    ;;
	*)
 	    echo "Unknown -m ${MODEL_TYPE}"
 	    echo "usage: $0 ${USAGE}"
 	    exit 1
	    ;;
    esac
}

function dolphin_prompt {
    PROMPT="<|im_start|>system
${SYSTEM_MESSAGE}<|im_end|>
<|im_start|>user
${QUESTION}
${INPUT}<|im_end|>
<|im_start|>assistant"
}

case "${MODEL_TYPE}" in
    ## Model: dolphin mixtral 8x7b
    dolphin|mixtral)
 	MODEL=~klotz/wip/llamafiles/models/dolphin-2.5-mixtral-8x7b.Q4_K_M.llamafile
 	MAX_CONTEXT_LENGTH=12288
	dolphin_prompt
	dolphin_priority
	;;

    ## Model: mistral-7b-instruct
    mistral)
	MODEL=$(find_first_file \
		    ~klotz/wip/llamafiles/models/mistral-7b-instruct-v0.1-Q4_K_M-main.llamafile \
		    ~klotz/wip/llamafiles/models/mistral-7b-instruct-v0.2.Q4_K_M.llamafile \
		    ~klotz/wip/llamafiles/models/mistral-7b-instruct-v0.2.Q5_K_M.llamafile)
	MAX_CONTEXT_LENGTH=7999
	PROMPT=$(printf "%b" "[INST]${SYSTEM_MESSAGE}\n${QUESTION}\n${INPUT}[/INST]\n")
	mixtral_priority
	;;
 
    ## Model: oobabooga/text-generation-webui/models/codebooga-34b-v0.1.Q4_K_M.gguf
    codebooga)
 	MODEL="~klotz/wip/llamafiles/bin/llamafile-main-0.1 -m ~klotz/wip/oobabooga/text-generation-webui/models/codebooga-34b-v0.1.Q4_K_M.gguf"
	MAX_CONTEXT_LENGTH=32768
 	PROMPT=$(printf "%b" "[INST]${SYSTEM_MESSAGE}\n${QUESTION}\n${INPUT}[/INST]\n")
 	SILENT_PROMPT=""	# not supported by codebooga
	codebooga_priority
	;;

    *)
	echo "unknown model type $MODEL_TYPE"
	exit 1
	;;
esac

if ! GPU=$(command -v nvidia-detector) || [[ "$GPU" == "None" ]]; then
    if [ "${DEBUG}" ]; then
	echo "* NO GPU"
    fi
    NGL=0
fi

if [ $(($PROMPT_LENGTH_EST * 2)) -gt "${CONTEXT_LENGTH}" ]; then
    echo "* WARNING: Prompt len $PROMPT_LENGTH_EST and response estimated not to fit in context ${CONTEXT_LENGTH}"
fi

if [ "$CONTEXT_LENGTH" -gt "$MAX_CONTEXT_LENGTH" ]; then
    CONTEXT_LENGTH="$MAX_CONTEXT_LENGTH"
    echo "* Truncated context length to $CONTEXT_LENGTH"
fi

if [ ! -f "${MODEL}" ]; then
    echo "Model not found: ${MODEL}"
    exit 1
fi

## Run
# -n 1000 ???
PROMPT_LENGTH_EST=$((${#PROMPT}/4))
if [ "${DEBUG}" ]; then
    printf '* Prompt; ngl=%s context_length=%s est_len=%s: %s' "${NGL}" "${CONTEXT_LENGTH}" "${PROMPT_LENGTH_EST}" "${PROMPT}"
    set -x
fi
# set -x
printf '%s' "${PROMPT}" | ${MODEL} --temp ${TEMPERATURE} -c ${CONTEXT_LENGTH} -ngl "${NGL}" --batch-size ${BATCH_SIZE} --no-penalize-nl --repeat-penalty 1 -t 10 -f /dev/stdin $SILENT_PROMPT 2> "${ERROR_OUTPUT}"
