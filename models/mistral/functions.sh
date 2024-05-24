#!/bin/bash

# Check if the script is being sourced or directly executed
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    echo "This script '${BASH_SOURCE[0]}' is intended to be sourced, not executed directly."
    exit 1
fi

function prepare_priority {
    MAX_CONTEXT_LENGTH=16384
    case "${PRIORITY}" in
        speed)
            NGL=${NGL:=33}
            CONTEXT_LENGTH=2048
            ;;
        length)
            NGL=${NGL:=33}
            CONTEXT_LENGTH=${MAX_CONTEXT_LENGTH}
            ;;
        manual)
            NGL=${NGL:=33}
            CONTEXT_LENGTH=${CONTEXT_LENGTH:=4096}
            ;;
        *)
            echo "usage: unknown priority $PRIORITY" >> /dev/stderr
            exit 1
            ;;
    esac

    cap_ngl
}

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

function prepare_model {
    MODEL=$(find_first_model \
                ${MODELS_DIRECTORY}/mistral/Mistral-7B-Instruct-v0.3-Q4_K_M.gguf \
                ${MODELS_DIRECTORY}/mistral/mistral-7b-instruct-v0.2.Q6_K.gguf \
                ${MODELS_DIRECTORY}/mistral/mistral-7b-instruct-v0.2.Q5_K_M.llamafile \
                ${MODELS_DIRECTORY}/mistral/mistral-7b-instruct-v0.2.Q5_K_M.gguf \
                ${MODELS_DIRECTORY}/mistral/mistral-7b-instruct-v0.2.Q4_K_M.llamafile \
                ${MODELS_DIRECTORY}/mistral/mistral-7b-instruct-v0.2.Q4_K_M.gguf \
                ${MODELS_DIRECTORY}/mistral/mistral-7b-instruct-v0.1-Q4_K_M-main.llamafile \
                ${MODELS_DIRECTORY}/mistral/mistral-7b-instruct-v0.2.Q3_K_M.llamafile \
                ${MODELS_DIRECTORY}/mistral/mistral-7b-instruct-v0.2.Q3_K_S.llamafile)
    gpu_check 4
    prepare_prompt
    prepare_priority
}
