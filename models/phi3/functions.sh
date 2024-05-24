#!/bin/bash

# Check if the script is being sourced or directly executed
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    echo "This script '${BASH_SOURCE[0]}' is intended to be sourced, not executed directly."
    exit 1
fi

# <|system|>
# You are a helpful AI assistant.<|end|>
# <|user|>
# How to explain Internet for a medieval knight?<|end|>
# <|assistant|>


function prepare_prompt {
    if [ "${INPUT}" == "" ];
    then
      printf -v PROMPT "<|system|>
%s
<|user|>
%s
<|assistant|>" "${SYSTEM_MESSAGE%$'\n'}" "${QUESTION%$'\n'}"
    else
      printf -v PROMPT "<|system|<>
%s
<|user|>
%s
%s
<|assistant|>" "${SYSTEM_MESSAGE%$'\n'}" "${QUESTION%$'\n'}" "${INPUT%$'\n'}"
    fi
}

function prepare_priority {
    MAX_CONTEXT_LENGTH=2048
    CONTEXT_LENGTH=${CONTEXT_LENGTH:=2048}
    BATCH_SIZE=${BATCH_SIZE:=128}
    NGL=${NGL:-33}
    cap_ngl
}

function prepare_model {
    MODEL=$(find_first_model \
                "${MODELS_DIRECTORY}/phi3/Phi-3-mini-4k-instruct.Q8_0.llamafile" \
         )
    gpu_check 4
    prepare_prompt
    prepare_priority
}
