#!/bin/bash

MODELS_PATHS="${MODELS_DIRECTORY}/nuextract/nuextract-q6_k.gguf"

# Check if the script is being sourced or directly executed
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    echo "This script '${BASH_SOURCE[0]}' is intended to be sourced, not executed directly."
    exit 1
fi

# override system message
SYSTEM_MESSAGE="Given the following JSON template and text, return a version of the JSON template filled in with the relevant data. Don't return anything besides the filled in JSON content."
#SYSTEM_MESSAGE="Given the following JSON template and text, return a version of the JSON template filled in with the relevant data:"

function prepare_prompt {
    printf -v PROMPT '<|input|>
%s

### Template:
%s

### Text:
%s
<|output|>' "${SYSTEM_MESSAGE%$'\n'}" "${QUESTION%$'\n'}" "${INPUT%$'\n'}"
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
    export LLM_ADDITIONAL_ARGS="${LLM_ADDITIONAL_ARGS} -r <|end-output|>"
}

# override this function from via/cli/functions.sh
function fixup_output {
    sed -e 's/<|end-output|>//'
}
