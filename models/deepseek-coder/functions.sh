#!/bin/bash

# Check if the script is being sourced or directly executed
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    echo "This script '${BASH_SOURCE[0]}' is intended to be sourced, not executed directly."
    exit 1
fi

function deepseek_coder_priority {
    MAX_CONTEXT_LENGTH=32768
    case "${PRIORITY}" in
         speed)
             NGL=${NGL:=33}
             CONTEXT_LENGTH=2048
             ;;
         length)
             NGL=${NGL:=25}
             CONTEXT_LENGTH=16383
             ;;
         manual)
            NGL=${NGL:=33}
            CONTEXT_LENGTH=${CONTEXT_LENGTH:=2048}
             ;;
        *)
             echo "usage: unknown priority $PRIORITY" >> /dev/stderr
             echo "usage: $0 ${USAGE}" >> /dev/stderr
             exit 1
            ;;
    esac

    cap_ngl
}

function deepseek_coder_model {
    MODEL=$(find_first_model \
                "${MODELS_DIRECTORY}/deepseek-coder/deepseek-coder-33b-instruct.Q5_K_M.gguf" \
                "${MODELS_DIRECTORY}/deepseek-coder/deepseek-coder-6.7b-instruct.Q4_K_M.gguf" \
         )
    SILENT_PROMPT=""
    gpu_check 2.1
    llama_prompt
    deepseek_coder_priority
}
