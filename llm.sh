#!/bin/bash

USAGE="[-m model-type] [--stdin] [--] QUESTION QUESTION QUESTION"

M="mistral"
INPUT=""
QUESTION=""
ERROR_OUTPUT="/dev/null"
SILENT_PROMPT="--silent-prompt"
TEMPERATURE="0"
CONTEXT_LENGTH="8192"

# If there are any args, require "--" or any non-hyphen word to terminate args and start question.
# Assume the whole args is a question if there's no hyphen to start.
if [[ "${1}" == "-"* ]]; then
    for ((index=1; index<$#; index ++)); do
	arg=${@:$index:1}
	if [[ "${arg}" == "-m" ]]; then
	    M="$2"; ((index ++));
	elif [[ "${arg}" != "-"* ]]; then
	    QUESTION=("${*:index + 1}")
	    break
	elif [[ "${arg}" == "--" ]]; then
	    QUESTION=("${*:index + 1}")
	    break
	elif [[ "${arg}" == "--stdin" ]]; then
	    if [ -t 0 ]; then
		echo "Give input followed by Ctrl-D:"
	    fi
	    INPUT=$(cat)
	elif [[ "${arg}" == "--debug" ]]; then
	  ERROR_OUTPUT="/dev/stdout"
	  SILENT_PROMPT=""
	fi
    done
else
    QUESTION="${*}"
fi

# Use environment variable to change SYSTEM_MESSAGE
SYSTEM_MESSAGE="${SYSTEM_MESSAGE-$(printf "%b" "Answer the following user question:\n")}"

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
	NGL=25
	;;

    ## Model: mistral-7b-instruct
    mistral)
	MODEL=/home/klotz/wip/llamafiles/mistral-7b-instruct-v0.1-Q4_K_M-main.llamafile
	PROMPT=$(printf "%b" "[INST]${SYSTEM_MESSAGE}\n${QUESTION}\n${INPUT}[/INST]\n")
	NGL=33
	;;

    ## Model: oobabooga/text-generation-webui/models/codebooga-34b-v0.1.Q4_K_M.gguf
    codebooga)
	MODEL="/home/klotz/wip/llamafiles/llamafile-main-0.1 -m /home/klotz/wip/oobabooga/text-generation-webui/models/codebooga-34b-v0.1.Q4_K_M.gguf"
	PROMPT=$(printf "%b" "[INST]${SYSTEM_MESSAGE}\n${QUESTION}\n${INPUT}[/INST]\n")
	SILENT_PROMPT=""	# not supported by codebooga
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

## Run
# -n 1000 ???
# printf '* Prompt is %s"' "${PROMPT}"
printf '%s' "${PROMPT}" | ${MODEL} --temp ${TEMPERATURE} -c ${CONTEXT_LENGTH} -ngl "${NGL}" -f /dev/stdin $SILENT_PROMPT 2> "${ERROR_OUTPUT}"
