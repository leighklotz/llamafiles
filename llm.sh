#!/bin/bash

USAGE="[-m model-type] [--stdin] [--] QUESTION QUESTION QUESTION"

M="mistral"
INPUT=""
QUESTION=""
ERROR_OUTPUT="/dev/null"
SILENT_PROMPT="--silent-prompt"
TEMPERATURE="0"
CONTEXT_LENGTH=16384
MAX_CONTEXT_LENGTH=${CONTEXT_LENGTH}
# Use environment variable to change SYSTEM_MESSAGE, as it is not a parameter
SYSTEM_MESSAGE="${SYSTEM_MESSAGE-$(printf "%b" "Answer the following user question:\n")}"

# If there are any args, require "--" or any non-hyphen word to terminate args and start question.
# Assume the whole args is a question if there's no hyphen to start.
if [[ "${1}" == "-"* ]]; then
    for ((index=1; index<$#; index ++)); do
	arg=${@:$index:1}
	if [[ "${arg}" == "-m" ]]; then
	    M="$2"; ((index ++));
	elif [[ "${arg}" == "-c" ]]; then
	    CONTEXT_LENGTH="$2"; ((index ++));
	elif [[ "${arg}" != "-"* ]]; then
	    QUESTION=("${*:index + 1}")
	    break
	elif [[ "${arg}" == "--" ]]; then
	    QUESTION=("${*:index + 1}")
	    break
	elif [[ "${arg}" == "--debug" ]]; then
	  ERROR_OUTPUT="/dev/stdout"
	  SILENT_PROMPT=""
	elif [[ "${arg}" == "--stdin" ]]; then
	    if [ -t 0 ]; then
		echo "Give input followed by Ctrl-D:"
	    fi
	    INPUT=$(cat)
	fi
    done
else
    QUESTION="${*}"
fi


case "${M}" in
    ## Model: dolphin mxtral 8x7b
    dolphin)
	MODEL=/home/klotz/wip/llamafiles/dolphin-2.5-mixtral-8x7b.Q4_K_M.llamafile
	PROMPT="<|im_start|>system
${SYSTEM_MESSAGE}<|im_end|>
<|im_start|>user
${QUESTION}
${INPUT}<|im_end|>
<|im_start|>assistant"
	# NGL=25
	NGL=20
	MAX_CONTEXT_LENGTH=32768
	;;

    ## Model: mistral-7b-instruct
    mistral)
	MODEL=/home/klotz/wip/llamafiles/mistral-7b-instruct-v0.1-Q4_K_M-main.llamafile
	PROMPT=$(printf "%b" "[INST]${SYSTEM_MESSAGE}\n${QUESTION}\n${INPUT}[/INST]\n")
	MAX_CONTEXT_LENGTH=8000
	NGL=33
	;;

    ## Model: oobabooga/text-generation-webui/models/codebooga-34b-v0.1.Q4_K_M.gguf
    codebooga)
	MODEL="/home/klotz/wip/llamafiles/llamafile-main-0.1 -m /home/klotz/wip/oobabooga/text-generation-webui/models/codebooga-34b-v0.1.Q4_K_M.gguf"
	PROMPT=$(printf "%b" "[INST]${SYSTEM_MESSAGE}\n${QUESTION}\n${INPUT}[/INST]\n")
	SILENT_PROMPT=""	# not supported by codebooga
	MAX_CONTEXT_LENGTH=16384
	NGL=40
	;;

    ## fail
    *)
	echo "usage: $0 ${USAGE}"
	exit 1
	;;
esac

if ! GPU=$(command -v nvidia-detector) || [[ "$GPU" == "None" ]]; then
    NGL=0
fi

if [ "$CONTEXT_LENGTH" -gt "$MAX_CONTEXT_LENGTH" ]; then
    CONTEXT_LENGTH="$MAX_CONTEXT_LENGTH"
    echo "* Truncated context length to $CONTEXT_LENGTH"
fi

## Run
# -n 1000 ???
# printf '* Prompt is %s"' "${PROMPT}"
printf '%s' "${PROMPT}" | ${MODEL} --temp ${TEMPERATURE} -c ${CONTEXT_LENGTH} -ngl "${NGL}" -f /dev/stdin $SILENT_PROMPT 2> "${ERROR_OUTPUT}"
