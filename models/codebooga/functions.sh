#!/bin/bash

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

function prepare_prompt {

    if [ "${INPUT}" == "" ]; then
        printf -v PROMPT "%s" "Below is an instruction that describes a task. Write a response that appropriately completes the request.

### Instruction:
${SYSTEM_MESSAGE%$'\n'}
${QUESTION%$'\n'}

### Response:
"
    else
        printf -v PROMPT "Below is an instruction that describes a task, paired with an input that provides further context. Write a response that appropriately completes the request.

### Instruction:
%s
%s

### Input:
%s

### Response:
" "${SYSTEM_MESSAGE%$'\n'}" "${QUESTION%$'\n'}" "${INPUT%$'\n'}"
        ##### END NO INPUT CASE
    fi
}


function prepare_model {
    MODEL=$(find_first_model \
                "${MODELS_DIRECTORY}/codebooga/codebooga-34b-v0.1.Q4_K_M.gguf" \
         )
    SILENT_PROMPT=""        # not supported by codebooga
    gpu_check 1
    prepare_prompt
    prepare_priority
}
