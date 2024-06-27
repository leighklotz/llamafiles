#!/bin/bash

# Check if the script is being sourced or directly executed
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    log_and_exit 1 "This script is intended to be sourced, not executed directly."
fi

# Find the first existing executable or GGUF in the list.
# file_path=$(find_first_model /path/to/file1 /path/to/file2 /path/to/file3)
function find_first_model() {
  local files=("$@")
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

# List all existing executable or GGUF in the list.
# file_path=$(find_first_model /path/to/file1 /path/to/file2 /path/to/file3)
function list_models() {
  local files=("$@")
  local file
  for file in "${files[@]}"; do
    if [ -f "$file" ] && ( [ -x "$file" ] || [ "${file##*.}" == "gguf" ] );
    then
      echo "${file}"
    fi
  done
  return 0
}

function list_model_types() {
    MODELS_DIRECTORY="$(realpath "${SCRIPT_DIR}/../models")"
    ls "${MODELS_DIRECTORY}"/*/functions.sh | xargs dirname | xargs -I {} basename {}
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

# todo: much work here
# load_model calls init_model so llm.sh can do prep work once it knows the model but before it is used
function init_via_model {
    echo "MODEL_TYPE=$MODEL_TYPE"
    if [ -z "${MODEL_TYPE}" ];
    then
	log_and_exit 1 "No MODEL_TYPE specified"
    fi

    # Construct the path to the functions file
    model_functions_path="$(realpath "${MODELS_DIRECTORY}/${MODEL_TYPE}/functions.sh")"
    source_functions "${model_functions_path}"
}

# any workarounds needed install here, e.g.
# sed -e 's/<img src="/<img  src="/g'
# by overriding this function in model/*/functions.sh
function fixup_input {
    cat
}

# any workarounds needed install here, e.g.
# by overriding this function in model/*/functions.sh
function fixup_output {
    cat
}

# todo: reduce global variables from llm.sh
# todo: make parallel with via_api_perform_inference
function cli_perform_inference {
    # Use llamafile or similar CLI runner to perform inference
    printf '%s' "${PROMPT}" > "${PROMPT_TEMP_FILE}"
    if [ -n "${GRAMMAR_FILE}" ]; then
	GRAMMAR_FILE="--grammar-file ${GRAMMAR_FILE}"
    fi
    #set -x
    cat "${PROMPT_TEMP_FILE}" | fixup_input | ${MODEL_RUNNER} ${MODEL_PATH} ${CLI_MODE} ${LOG_DISABLE} ${GPU} ${NGL} ${GRAMMAR_FILE} ${TEMPERATURE} ${CONTEXT_LENGTH} ${N_PREDICT} ${BATCH_SIZE} ${NO_PENALIZE_NL} --repeat-penalty 1 ${THREADS} -f /dev/stdin ${SILENT_PROMPT} --seed "${SEED}" ${LLM_ADDITIONAL_ARGS} | fixup_output 2> "${ERROR_OUTPUT}"
    return $?
}

function cli_set_model_path {
    #set -x
    if [ -z "${MODEL_PATH}" ];
    then
	local models_paths=${@}
	log_info "* models_paths = ${models_paths}"
	MODEL_PATH=$(find_first_model ${models_paths[@]})
    fi
    if [ -z "${MODEL_PATH}" ]; then
	log_and_exit 1 "Cannot find model for MODEL_TYPE=$MODEL_TYPE in $MODELS_DIRECTORY"
    fi
}

# if [ -n "${VIA_CLI_FUNCTIONS_LOADED}" ]; then
#     log_and_exit "VIA_CLI_FUNCTIONS_LOADED again"
# else
#     VIA_CLI_FUNCTIONS_LOADED=1
#     log_error "VIA_CLI_FUNCTIONS_LOADED first_time"
# fi

