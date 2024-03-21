#!/bin/bash

# Check if the script is being sourced or directly executed
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    echo "This script '${BASH_SOURCE[0]}' is intended to be sourced, not executed directly."
    exit 1
fi

function nous_hermes_prompt {
    chatml_prompt
}

function nous_hermes_priority {
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

function nous_hermes_model {
    MODEL=$(find_first_model \
                ${MODELS_DIRECTORY}/nous-hermes/Nous-Hermes-2-Mixtral-8x7B-DPO.Q4_K_M.gguf \
                ${MODELS_DIRECTORY}/nous-hermes/nous-hermes-2-mixtral-8x7b-dpo.Q5_K_M.gguf \
         )
    gpu_check 1
    nous_hermes_prompt
    nous_hermes_priority
}

