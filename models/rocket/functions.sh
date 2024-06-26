#!/bin/bash

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


function set_model_path {
    if [ -z "${MODEL_PATH}" ];
    then
        MODEL_PATH=$(find_first_model \
			 "${MODELS_DIRECTORY}/rocket/rocket-3b.Q6_K.llamafile" \
			 "${MODELS_DIRECTORY}/rocket/rocket-3b.Q5_K_M.llamafile" \
			 "${MODELS_DIRECTORY}/rocket/rocket-3b.Q4_K_M.llamafile" \
		  )
    fi
    if [ "${MODEL_PATH}" == "" ]; then
	log_and_exit 1 "Cannot find model for MODEL_TYPE=$MODEL_TYPE in $MODELS_DIRECTORY"
    fi
}

function get_model_name {
    set_model_path
    basename "${MODEL_PATH}"
}

function prepare_model {
    set_model_path
    USE_SYSTEM_ROLE=1
    gpu_check 4
    chatml_prompt
    prepare_priority
    prepare_overrides
}
