#!/bin/bash

# Check if the script is being sourced or directly executed
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    echo "This script '${BASH_SOURCE[0]}' is intended to be sourced, not executed directly."
    exit 1
fi

function prepare_prompt {
    PROMPT="<s>[INST]"
    local system_message="${SYSTEM_MESSAGE%$'\n'}"
    local input="${INPUT%$'\n'}"
    local question="${QUESTION%$'\n'}"
    [ -n "${system_message}" ] && printf -v PROMPT "%s%s\n" "${PROMPT}" "${system_message}"
    [ -n "${question}" ] &&       printf -v PROMPT "%s\n%s" "${PROMPT}" "${question}"
    [ -n "${input}" ] &&          printf -v PROMPT "%s\n%s" "${PROMPT}" "${input}"
                                  printf -v PROMPT "%s[/INST]" "${PROMPT}"
}

function prepare_priority {
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

function set_model_path {
    if [ -z "${MODEL_PATH}" ];
    then
	MODEL_PATH="$(find_first_model \
			 ${MODELS_DIRECTORY}/mixtral/mixtral-8x7b-instruct-v0.1.Q5_K_M.gguf \
			 ${MODELS_DIRECTORY}/mixtral/mixtral-8x7b-instruct-v0.1.Q5_K_M.llamafile \
			 ${MODELS_DIRECTORY}/mixtral/mixtral_7bx2_moe.Q3_K_M.gguf \
		  )"
    fi
}

function get_model_name {
    set_model_path
    basename "${MODEL_PATH}"
}

function prepare_model {
    set_model_path
    gpu_check 1.5
    prepare_prompt
    prepare_priority
}

