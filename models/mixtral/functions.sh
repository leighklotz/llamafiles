#!/bin/bash

# Check if the script is being sourced or directly executed
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    echo "This script '${BASH_SOURCE[0]}' is intended to be sourced, not executed directly."
    exit 1
fi

function mixtral_prompt {
    if [ "${INPUT}" == "" ]; then
	printf -v PROMPT "<s>[INST] %s
%s [/INST]
" "${SYSTEM_MESSAGE%$'\n'}" "${QUESTION%$'\n'}"
    else
	printf -v PROMPT "<s>[INST] %s
%s
%s [/INST]
" "${SYSTEM_MESSAGE%$'\n'}" "${QUESTION%$'\n'}" "${INPUT%$'\n'}"
    fi
}

function mixtral_priority {
    MAX_CONTEXT_LENGTH=21000
    case "${PRIORITY}" in
         speed)
             NGL=${NGL:=22}
             CONTEXT_LENGTH=2048
             ;;
         length)
             NGL=${NGL:=20}
             CONTEXT_LENGTH=${MAX_CONTEXT_LENGTH}
             ;;
         manual)
             NGL=${NGL:=22}
             CONTEXT_LENGTH=${CONTEXT_LENGTH:=4096}
             ;;
         *)
             echo "usage: unknown priority $PRIORITY" >> /dev/stderr
             exit 1
            ;;
    esac
    cap_ngl
}

function mixtral_model {
    MODEL=$(find_first_model \
                ${MODELS_DIRECTORY}/mixtral/mixtral-8x7b-instruct-v0.1.Q5_K_M.gguf \
                ${MODELS_DIRECTORY}/mixtral/mixtral-8x7b-instruct-v0.1.Q5_K_M.llamafile \
                ${MODELS_DIRECTORY}/mixtral/mixtral_7bx2_moe.Q3_K_M.gguf \
         )
    gpu_check 1.5
    mixtral_prompt
    mixtral_priority
}

