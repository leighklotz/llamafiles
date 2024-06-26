#!/bin/bash

MODELS_PATHS="${MODELS_DIRECTORY}/cerebrum/Cerebrum-1.0-7b-Q6_K.gguf"

# Check if the script is being sourced or directly executed
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    echo "This script '${BASH_SOURCE[0]}' is intended to be sourced, not executed directly."
    exit 1
fi

function prepare_priority {
    MAX_CONTEXT_LENGTH=16384
    case "${PRIORITY}" in
        speed)
            NGL=${NGL:=33}      # 20 for 8x7b model
            CONTEXT_LENGTH=2048
            ;;
        length)
            NGL=${NGL:=20}
            CONTEXT_LENGTH=${MAX_CONTEXT_LENGTH}
            ;;
        manual)
            NGL=${NGL:=20}
            CONTEXT_LENGTH=${CONTEXT_LENGTH:=4096}
            ;;
        *)
            echo "usage: unknown priority $PRIORITY" >> /dev/stderr
            exit 1
            ;;
    esac

    cap_ngl
}


# Template from https://huggingface.co/AetherResearch/Cerebrum-1.0-7b
# <s>A chat between a user and a thinking artificial intelligence assistant. The assistant describes its thought process and gives helpful and detailed answers to the user's questions.
# User: Are you conscious?
# AI:

function prepare_prompt {
    if [ "${INPUT}" == "" ]; then
        printf -v PROMPT "<s>A chat between a user and a thinking artificial intelligence assistant. The assistant describes its thought process and gives helpful and detailed answers to the user's questions. %s
User: %s
AI:
" "${SYSTEM_MESSAGE%$'\n'}" "${QUESTION%$'\n'}"
    else
        printf -v PROMPT "<s>A chat between a user and a thinking artificial intelligence assistant. The assistant describes its thought process and gives helpful and detailed answers to the user's questions. 
%s
User: %s
INPUT: %s
AI: " "${SYSTEM_MESSAGE%$'\n'}" "${QUESTION%$'\n'}" "${INPUT%$'\n'}"
    fi
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
