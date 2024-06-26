#!/bin/bash

MODELS_PATHS="${MODELS_DIRECTORY}/codebooga/codebooga-34b-v0.1.Q4_K_M.gguf"

# Check if the script is being sourced or directly executed
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    echo "This script '${BASH_SOURCE[0]}' is intended to be sourced, not executed directly."
    exit 1
fi

function prepare_priority {
    MAX_CONTEXT_LENGTH=32768
    case "${PRIORITY}" in
         speed)
             NGL=${NGL:=50}
             CONTEXT_LENGTH=4096
             ;;
         length)
             NGL=${NGL:=16}
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

function get_model_name {
    cli_set_model_path ${MODELS_PATHS}
    basename "${MODEL_PATH}"
}

function prepare_model {
    cli_set_model_path ${MODELS_PATHS}
    gpu_check 1
    alpaca_prompt
    prepare_priority
}
