#!/bin/bash

# Check if the script is being sourced or directly executed
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    echo "This script '${BASH_SOURCE[0]}' is intended to be sourced, not executed directly."
    exit 1
fi

function phi_prompt {
    if [ "${INPUT}" == "" ];
    then
      printf -v PROMPT "Instruct: %s
%s
Output:" "${SYSTEM_MESSAGE%$'\n'}" "${QUESTION%$'\n'}"
    else
      printf -v PROMPT "Instruct: %s
%s

Output:" "${SYSTEM_MESSAGE%$'\n'}" "${QUESTION%$'\n'}" "${INPUT%$'\n'}"
    fi
}

function phi_priority {
    MAX_CONTEXT_LENGTH=2048
    CONTEXT_LENGTH=${CONTEXT_LENGTH:=2048}
    BATCH_SIZE=${BATCH_SIZE:=128}
    NGL=${NGL:-33}
    cap_ngl
}

function phi_model {
    MODEL=$(find_first_model \
                "${MODELS_DIRECTORY}/phi/phi-2.Q6_K.llamafile" \
                "${MODELS_DIRECTORY}/phi/phi-2.Q5_K_M.llamafile" \
         )
    gpu_check 4
    phi_prompt
    phi_priority
}
