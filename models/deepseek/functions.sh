#!/bin/bash

MODELS_PATHS="${MODELS_DIRECTORY}/deepseek/DeepSeek-Coder-V2-Lite-Instruct-Q6_K_L.gguf"

# Check if the script is being sourced or directly executed
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    echo "This script '${BASH_SOURCE[0]}' is intended to be sourced, not executed directly."
    exit 1
fi

function prepare_priority {
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

function get_model_name {
    cli_set_model_path ${MODELS_PATHS}
    basename "${MODEL_PATH}"
}

# """<｜begin▁of▁sentence｜>{system_prompt}
# 
# User: {prompt}
# 
# Assistant: <｜end▁of▁sentence｜>"""


function deepseek_coder_v2_prompt {
    local system_message="${SYSTEM_MESSAGE%$'\n'}"
    local question="${QUESTION%$'\n'}"
    local input="${INPUT%$'\n'}"
    local BOS="<｜begin▁of▁sentence｜>"
    local EOS="<｜end▁of▁sentence｜>"
    # deepseek_coder_v2 does not support system role but it has a place for me
    # see if it works
    printf -v PROMPT "${BOS}%s\n" "${system_message}"

    if [ -n "${input}" ];then
	printf -v PROMPT "%s\nUser: %s\n%s\n" "${PROMPT}" "${question}" "${input}"
    else
	printf -v PROMPT "%s\nUser: %s\n" "${PROMPT}" "${question}" 
    fi
    printf -v PROMPT "%s\nAssistant: ${EOS}\n" "${PROMPT}"
}

function prepare_model {
    cli_set_model_path ${MODELS_PATHS[@]}
    gpu_check 2.1
    deepseek_coder_v2_prompt
    prepare_priority
}
