#!/bin/bash

USAGE="[-m model-type] [--stdin] [--] QUESTION QUESTION QUESTION"

# Use flags or environment variables below:
MODEL_TYPE=${MODEL_TYPE:-mistral}
INPUT=${INPUT:-}
QUESTION=${QUESTION:-}
ERROR_OUTPUT="/dev/null"
TEMPERATURE=${TEMPERATURE:-0.33}
CONTEXT_LENGTH=${CONTEXT_LENGTH:-8000}
MAX_CONTEXT_LENGTH=${CONTEXT_LENGTH}
SYSTEM_MESSAGE="${SYSTEM_MESSAGE-$(printf "%b" "Answer the following user question:\n")}"
SILENT_PROMPT="--silent-prompt"
BATCH_SIZE=${BATCH_SIZE:-$(($CONTEXT_LENGTH / 2))}
NGL=-1
PRIORITY="manual" # manual|speed|context

# memory allocation: assume 4 chars per token
PROMPT_LENGTH_EST=$(((75+${#SYSTEM_MESSAGE}+${#QUESTION}+${#INPUT})/4))

# echo "PROMPT_LENGTH_EST=$PROMPT_LENGTH_EST"

# If there are any args, require "--" or any non-hyphen word to terminate args and start question.
# Assume the whole args is a question if there is no hyphen to start.
if [[ "${1}" == "-"* ]]; then
    for ((index=1; index<$#; index ++)); do
	arg=${@:$index:1}
	if [[ "${arg}" == "-m" ]]; then
	    ((index ++));
	    MODEL_TYPE="${@:$index:1}"
	elif [[ "${arg}" == "--priority" ]]; then
	    ((index ++));
	    PRIORITY="${@:$index:1}"
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
 	    NGL=${NGL:-33}
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
	    NGL=33
	    CONTEXT_LENGTH=2048
	    ;;
	context)
	    NGL=8
	    CONTEXT_LENGTH=7999
	    ;;
	manual)
	    NGL=${NGL:-33}
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
 	    NGL=${NGL:-33}
 	    ;;
 	*)
 	    echo "usage: unknown priority $PRIORITY"
 	    exit 1
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
    ## Model: dolphin mxtral 8x7b
    dolphin)
 	MODEL=/home/klotz/wip/llamafiles/dolphin-2.5-mixtral-8x7b.Q4_K_M.llamafile
 	MAX_CONTEXT_LENGTH=12288
	dolphin_prompt
	dolphin_priority
	;;

    ## Model: mistral-7b-instruct
    mistral)
	MODEL=/home/klotz/wip/llamafiles/mistral-7b-instruct-v0.1-Q4_K_M-main.llamafile
	MAX_CONTEXT_LENGTH=7999
	PROMPT=$(printf "%b" "[INST]${SYSTEM_MESSAGE}\n${QUESTION}\n${INPUT}[/INST]\n")
	mixtral_priority
	;;
 
    ## Model: oobabooga/text-generation-webui/models/codebooga-34b-v0.1.Q4_K_M.gguf
    codebooga)
 	MODEL="/home/klotz/wip/llamafiles/llamafile-main-0.1 -m /home/klotz/wip/oobabooga/text-generation-webui/models/codebooga-34b-v0.1.Q4_K_M.gguf"
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
    echo "NO GPU"
    NGL=0
fi

if [ $(($PROMPT_LENGTH_EST * 2)) -gt "${CONTEXT_LENGTH}" ]; then
    echo "* WARNING: Prompt len $PROMPT_LENGTH_EST and response estimated not to fit in context ${CONTEXT_LENGTH}"
fi

if [ "$CONTEXT_LENGTH" -gt "$MAX_CONTEXT_LENGTH" ]; then
    CONTEXT_LENGTH="$MAX_CONTEXT_LENGTH"
    echo "* Truncated context length to $CONTEXT_LENGTH"
fi

## Run
# -n 1000 ???
PROMPT_LENGTH_EST=$((${#PROMPT}/4))

# printf '* Prompt; ngl=%s context_length=%s est_len=%s: %s' "${NGL}" "${CONTEXT_LENGTH}" "${PROMPT_LENGTH_EST}" "${PROMPT}"
set -x
printf '%s' "${PROMPT}" | ${MODEL} --temp ${TEMPERATURE} -c ${CONTEXT_LENGTH} -ngl "${NGL}" --batch-size ${BATCH_SIZE} --no-penalize-nl --repeat-penalty 1 -t 10 -f /dev/stdin $SILENT_PROMPT 2> "${ERROR_OUTPUT}"
