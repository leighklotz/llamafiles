#!/bin/bash

# Check if the script is being sourced or directly executed
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    echo "This script '${BASH_SOURCE[0]}' is intended to be sourced, not executed directly."
    exit 1
fi

# Dolphin of various sorts

function dolphin_priority {
    MAX_CONTEXT_LENGTH=32768
    case "${PRIORITY}" in
         speed)
             NGL=${NGL:=28}
             CONTEXT_LENGTH=2048
             ;;
         length)
             NGL=${NGL:=8}
             CONTEXT_LENGTH=${MAX_CONTEXT_LENGTH}
             ;;
         manual)
             NGL=${NGL:=23}
             CONTEXT_LENGTH=${CONTEXT_LENGTH:=4096}
             ;;
         *)
             echo "usage: unknown priority $PRIORITY" >> /dev/stderr
             exit 1
            ;;
    esac
    cap_ngl
}

function prepare_model {
    MODEL=$(find_first_model \
                ${MODELS_DIRECTORY}/dolphin/dolphin-2.7-mixtral-8x7b.Q4_K_M.gguf \
                ${MODELS_DIRECTORY}/dolphin/dolphin-2.7-mixtral-8x7b.Q4_K_M.gguf \
         )
    gpu_check 1.3
    chatml_prompt
    dolphin_priority
}
