#!/bin/bash

# args, "[-m model-type]
if [[ "${1}" == "-m" ]]; then
    shift; M=$1; shift;
else
    M="mistral"
fi

QUESTION="${*}"
SYSTEM_MESSAGE=$(printf "%b" "Answer the following user question about Linux, bash, python, or other programming:\n")

case "${M}" in
    ## Model: dolphin mxtral 8x7b
    dolphin)
	MODEL=/home/klotz/wip/llamafile/dolphin-2.5-mixtral-8x7b.Q4_K_M.llamafile
PROMPT="<|im_start|>system
${SYSTEM_MESSAGE}<|im_end|>
<|im_start|>user
${QUESTION}<|im_end|>
<|im_start|>assistant"
	NGL=25
	SILENT="--silent-prompt"
	;;

    ## Model: mistral-7b-instruct
    mistral)
	MODEL=/home/klotz/wip/llamafile/mistral-7b-instruct-v0.1-Q4_K_M-main.llamafile
	PROMPT="[INST]${SYSTEM_MESSAGE}${QUESTION}[/INST]"
	NGL=33
	SILENT="--silent-prompt"
	;;

    ## Model: oobabooga/text-generation-webui/models/codebooga-34b-v0.1.Q4_K_M.gguf
    codebooga)
	MODEL="/home/klotz/wip/llamafile-main-0.1 -m /home/klotz/wip//oobabooga/text-generation-webui/models/codebooga-34b-v0.1.Q4_K_M.gguf"
	PROMPT="[INST]${SYSTEM_MESSAGE}${QUESTION}[/INST]"
	NGL=40
	;;
    ## Debug Model
    debug)
	MODEL="echo model"
	PROMPT=$(printf "%b" "S=${SYSTEM_MESSAGE} Q=${QUESTION}")
	NGL=0
	SILENT="--silent-prompt"
	;;

    ## fail
    *)
	echo "usage: $0 [-m model-type] question words"
	exit 1
	;;
esac

if ! GPU=$(command -v nvidia-detector) || [[ "$GPU" == "None" ]]; then
    NGL=0
fi

## Run
#  -n 1000 ???
${MODEL} --temp 0 -c 6000 -ngl "${NGL}" -p "${PROMPT}" "${SILENT}" 2>/dev/null 
