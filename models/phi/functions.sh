#!/bin/bash

MODELS_PATHS="${MODELS_DIRECTORY}/phi/phi-2.Q6_K.llamafile \
              ${MODELS_DIRECTORY}/phi/phi-2.Q5_K_M.llamafile"

# Check if the script is being sourced or directly executed
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    echo "This script '${BASH_SOURCE[0]}' is intended to be sourced, not executed directly."
    exit 1
fi

function prepare_prompt {
    if [ "${INPUT}" == "" ];
    then
      printf -v PROMPT "Instruct: %s
%s
Output:" "${SYSTEM_MESSAGE%$'\n'}" "${QUESTION%$'\n'}"
    else
      printf -v PROMPT "Instruct: %s
%s
%s
Output:
" "${SYSTEM_MESSAGE%$'\n'}" "${QUESTION%$'\n'}" "${INPUT%$'\n'}"
    fi
}

function prepare_priority {
    MAX_CONTEXT_LENGTH=2048
    CONTEXT_LENGTH=${CONTEXT_LENGTH:=2048}
    BATCH_SIZE=${BATCH_SIZE:=128}
    NGL=${NGL:-33}
    cap_ngl
}

function get_model_name {
    cli_set_model_path ${MODELS_PATHS}
    basename "${MODEL_PATH}"
}

function prepare_model {
    cli_set_model_path ${MODELS_PATHS}
    gpu_check 4
    prepare_prompt
    prepare_priority
}
