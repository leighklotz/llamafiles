#!/bin/bash

# Check if the script is being sourced or directly executed
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    echo "This script '${BASH_SOURCE[0]}' is intended to be sourced, not executed directly."
    exit 1
fi

function cerebrum_priority {
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

function cerebrum_prompt {
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

# todo: it's odd to mix of mistral-7b and mixtral-8x7b but they have the same prompt
# unforunately 7b can have 33 layers but 8x7b only 20
#               ${MODELS_DIRECTORY}/cerebrum/cerebrum-1.0-8x7b_q6_k.gguf 

function prepare_model {
    MODEL=$(find_first_model \
                ${MODELS_DIRECTORY}/cerebrum/Cerebrum-1.0-7b-Q6_K.gguf \
         )
    gpu_check 4
    cerebrum_prompt
    cerebrum_priority
}
