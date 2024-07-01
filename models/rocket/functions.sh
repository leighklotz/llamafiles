#!/bin/bash

MODELS_PATHS="${MODELS_DIRECTORY}/phi/phi-2.Q6_K.llamafile\
              ${MODELS_DIRECTORY}/phi/phi-2.Q5_K_M.llamafile"


# Check if the script is being sourced or directly executed
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    echo "This script '${BASH_SOURCE[0]}' is intended to be sourced, not executed directly."
    exit 1
fi

function prepare_priority {
    MAX_CONTEXT_LENGTH=4096
    case "${PRIORITY}" in
         speed)
             NGL=${NGL:=33}
             CONTEXT_LENGTH=2048
             ;;
         length)
             NGL=${NGL:=25}
             CONTEXT_LENGTH=4096
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

function prepare_overrides {
    if [ -n "${SILENT_PROMPT}" ] && [[ "${SILENT_PROMPT}" =~ '--no-display-prompt' ]];
    then
        SILENT_PROMPT="$(printf "%s" "${SILENT_PROMPT}" | sed -e 's/ *--no-display-prompt//g')"
    fi
}

function get_model_name {
    cli_set_model_path ${MODELS_PATHS}
    basename "${MODEL_PATH}"
}

function prepare_model {
    cli_set_model_path ${MODELS_PATHS}
    USE_SYSTEM_ROLE=1
    gpu_check 4
    chatml_prompt
    prepare_priority
    prepare_overrides
}
