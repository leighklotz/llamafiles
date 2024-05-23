#!/bin/bash

function log_verbose {
    local prog="$(basename "$0")"
    local message="$1"
    if [ "${VERBOSE}" != '' ];
       then
	   printf "* %s: %s\n" "${prog}" "${message}" > /dev/stderr
    fi
}

function log_debug {
    local prog="$(basename "$0")"
    local message="$1"
    if [ "${DEBUG}" != '' ];
       then
	   printf "* %s: %s\n" "${prog}" "${message}" > /dev/stderr
    fi
}

function log_info {
    local prog="$(basename "$0")"
    local message="$1"
    printf "* %s: %s\n" "${prog}" "${message}" > /dev/stderr
}

function log_warn {
    local prog="$(basename "$0")"
    local code=$1
    local message="$2"
    printf "* WARN %s (%s): %s\n" "${prog}" "${code}" "${message}" > /dev/stderr
}

function log_error {
    local prog="$(basename "$0")"
    local message="$1"
    printf "* ERROR %s: %s\n" "${prog}" "${message}" > /dev/stderr
}

function log_and_exit {
    local prog="$(basename "$0")"
    local code=$1
    local message="$2"
    printf "* ERROR %s (%s): %s\n" "${prog}" "${code}" "${message}" > /dev/stderr
    [[ $code =~ ^[0-9]+$ ]] && exit $code || exit 1
}

# Check if the script is being sourced or directly executed
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    log_and_exit 1 "This script is intended to be sourced, not executed directly."
fi

# Find the first existing executable or GGUF in the list.
# file_path=$(find_first_model /path/to/file1 /path/to/file2 /path/to/file3)
function find_first_model() {
  local files=("$@")
  local file
  for file in "${files[@]}"; do
    log_verbose "Checking Model $file"
    if [ -f "$file" ] && ( [ -x "$file" ] || [ "${file##*.}" == "gguf" ] );
    then
      log_verbose "Accepting Model $file"
      echo "${file}"
      return 0
    fi
  done
  log_error "Cannot find executable model in $@"
  return 1
}

# Prompt Markup
function alpaca_prompt {

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

function llama_prompt {
    if [ "${INPUT}" == "" ]; then
	printf -v PROMPT "<s> [INST] %s
%s [/INST]
" "${SYSTEM_MESSAGE%$'\n'}" "${QUESTION%$'\n'}"
    else
	printf -v PROMPT "<s>[INST]%s
%s
%s
[/INST]
" "${SYSTEM_MESSAGE%$'\n'}" "${QUESTION%$'\n'}" "${INPUT%$'\n'}"
    fi
}

function alpaca_prompt {
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

function chatml_prompt {
    local system_message="${SYSTEM_MESSAGE%$'\n'}"
    local question="${QUESTION%$'\n'}"
    local input="${INPUT%$'\n'}"
    PROMPT=""

    if [ -n "${USE_SYSTEM_ROLE}" ];
    then
	printf -v PROMPT '%s<|im_start|>system\n%s<|im_end|>\n' "${PROMPT}" "${system_message}"
    else
	# fixme: we just use two 'user' messages with no 'assistant' message
	#        it might be better to prepend system message to the question+input, ending with newline
	printf -v PROMPT '%s<|im_start|>user\n%s<|im_end|>\n' "${PROMPT}" "${system_message}"
    fi

    if [ -n "${input}" ];
    then
	printf -v PROMPT '%s<|im_start|>user\n%s\n%s<|im_end|>\n' "${PROMPT}" "${question}" "${input}"
    else
	printf -v PROMPT '%s<|im_start|>user\n%s<|im_end|>\n' "${PROMPT}" "${question}"
    fi
    printf -v PROMPT '%s<|im_start|>assistant' "${PROMPT}"
}
function chatml_prompt {
    local system_message="${SYSTEM_MESSAGE%$'\n'}"
    local question="${QUESTION%$'\n'}"
    local input="${INPUT%$'\n'}"
    PROMPT=""

    if [ -n "${USE_SYSTEM_ROLE}" ];
    then
	printf -v PROMPT '%s<|im_start|>system\n%s<|im_end|>\n' "${PROMPT}" "${system_message}"
    else
	# fixme: we just use two 'user' messages with no 'assistant' message
	#        it might be better to prepend system message to the question+input, ending with newline
	printf -v PROMPT '%s<|im_start|>user\n%s<|im_end|>\n' "${PROMPT}" "${system_message}"
    fi

    if [ -n "${input}" ];
    then
	printf -v PROMPT '%s<|im_start|>user\n%s\n%s<|im_end|>\n' "${PROMPT}" "${question}" "${input}"
    else
	printf -v PROMPT '%s<|im_start|>user\n%s<|im_end|>\n' "${PROMPT}" "${question}"
    fi
    printf -v PROMPT '%s<|im_start|>assistant' "${PROMPT}"
}

# todo: much work here
# load_model and prepare_model are two functions so llm.sh can do prep work once it knows the model but before it is loaded
function load_model {
    if [ -z "${MODEL_TYPE}" ];
    then
	log_and_exit 3 "Model not found: ${MODEL_TYPE}"
    fi

    # Construct the path to the functions file
    model_functions_path="$(realpath "${MODELS_DIRECTORY}/${MODEL_TYPE}/functions.sh")"

    # Check if the model functions file exists
    if [[ -f "${model_functions_path}" ]]; then
	source "${model_functions_path}"
    else
	log_and_exit 1 "Cannot find model functions for ${MODEL_TYPE}: ${model_functions_path}"
    fi

}
