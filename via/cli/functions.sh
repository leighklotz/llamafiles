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
  log_error "Cannot find executable model in $*"
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
    if [ -n "${ADD_BOS}" ]; then
	BOS='<s>'
    else
	BOS=''
    fi

    if [ "${INPUT}" == "" ]; then
	printf -v PROMPT '%s[INST] %s
%s [/INST] ' "${BOS}" "${SYSTEM_MESSAGE%$'\n'}" "${QUESTION%$'\n'}"
    else
	printf -v PROMPT '%s[INST] %s
%s
%s [/INST] ' "${BOS}" "${SYSTEM_MESSAGE%$'\n'}" "${QUESTION%$'\n'}" "${INPUT%$'\n'}"
    fi
}

function alpaca_prompt {
    if [ "${INPUT}" == "" ]; then
	printf -v PROMPT 'Below is an instruction that describes a task. Write a response that appropriately completes the request.

### Instruction:
%s
%s

### Response:
' "${SYSTEM_MESSAGE%$'\n'}" "${QUESTION%$'\n'}"
    else
	printf -v PROMPT 'Below is an instruction that describes a task, paired with an input that provides further context. Write a response that appropriately completes the request.

### Instruction:
%s
%s

### Input:
%s

### Response:
' "${SYSTEM_MESSAGE%$'\n'}" "${QUESTION%$'\n'}" "${INPUT%$'\n'}"
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
    if [ -z "${MODEL_TYPE}" ];
    then
	log_and_exit 1 "No MODEL_TYPE specified"
    fi

    # Construct the path to the functions file
    log_info "* init_via_model ${MODELS_DIRECTORY}/${MODEL_TYPE}/functions.sh"
    model_functions_path="$(realpath "${MODELS_DIRECTORY}/${MODEL_TYPE}/functions.sh")"
    source_functions "${model_functions_path}"

    set_threads
    set_model_runner
}

function set_model_runner {
    # set MODEL_RUNNER
    if [ "${MODEL##*.}" == "gguf" ] || [ "${FORCE_MODEL_RUNNER}" ];
    then
	MODEL_RUNNER="${LLAMAFILE_MODEL_RUNNER}"
    fi
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

# Calculate $LLM_SH command line options
# Use bash :+ syntax to avoid setting prefixes on empty values
function via_set_options {
    N_PREDICT="${N_PREDICT:+--n-predict $N_PREDICT}"
    TEMPERATURE="${TEMPERATURE:+--temp $TEMPERATURE}"
    CONTEXT_LENGTH="${CONTEXT_LENGTH:+-c $CONTEXT_LENGTH}"
    BATCH_SIZE="${BATCH_SIZE:+--batch-size $BATCH_SIZE}"
    NGL="${NGL:+-ngl $NGL}"
    GPU="${GPU:+--gpu $GPU}"
}


# todo: reduce global variables from llm.sh
# todo: make parallel with via_api_perform_inference
function cli_perform_inference {
    # Use llamafile or similar CLI runner to perform inference
    printf '%s' "${PROMPT}" > "${PROMPT_TEMP_FILE}"
    if [ -n "${GRAMMAR_FILE}" ]; then
	GRAMMAR_FILE="--grammar-file ${GRAMMAR_FILE}"
    fi
    # set -x
    cat "${PROMPT_TEMP_FILE}" | fixup_input | ${MODEL_RUNNER} "${MODEL_PATH}" --cli ${LOG_DISABLE} ${GPU} ${NGL} ${GRAMMAR_FILE} ${TEMPERATURE} ${CONTEXT_LENGTH} ${N_PREDICT} ${BATCH_SIZE} ${NO_PENALIZE_NL} --repeat-penalty 1 ${THREADS} -f /dev/stdin ${SILENT_PROMPT} --seed "${SEED}" ${LLM_ADDITIONAL_ARGS} | fixup_output 2> "${ERROR_OUTPUT}"
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

function cap_ngl {
    if [ "$GPU" != "none" ] && [ -n "$GPU" ] && [ -n "${NGL}" ] && [ "${NGL}" -gt "${MAX_NGL_EST}" ];
    then
        log_verbose "* Capping $NGL at $MAX_NGL_EST"
        NGL=$MAX_NGL_EST
    fi
}

function set_threads() {
    # Get thread count
    if [ -z "${THREADS}" ];
    then
	THREADS=$( ( [ -f /proc/cpuinfo ] && grep '^cpu cores\s*:' /proc/cpuinfo | head -1 | awk '{print $4}' ))
	if [ -z "${THREADS}" ];
	then
            THREADS=$(sysctl -n hw.ncpu 2>/dev/null || echo "${NUMBER_OF_PROCESSORS:-4}")
	fi
    fi
    THREADS="${THREADS:+-t $THREADS}"
}

function gpu_check {
    local layer_per_gb="$1"

    if [ -z "${layer_per_gb}" ];
    then
        layer_per_gb=1
    fi
    if [ "${GPU}" == "none" ] || ! gpu_detector=$(command -v nvidia-detector) || [[ "$($gpu_detector)" == "None" ]];
    then
        if [ "${DEBUG}" ];
        then
            log_debug "NO GPU"
        fi
        FREE_VRAM_GB=0
        MAX_NGL_EST=0
        NGL=0
        GPU=""
    else
        # if gpu is already in use, estimate NGL max at int(free_vram_gb * 1.5)
        FREE_VRAM_GB=$(nvidia-smi --query-gpu=memory.free --format=csv,nounits,noheader | awk '{print $1 / 1024}')
        if (( $(echo "${FREE_VRAM_GB} < 2" |bc -l) ));
        then
            GPU=""
            MAX_NGL_EST=0
            NGL=0
        else
            GPU="nvidia"
            MAX_NGL_EST=$(awk -vfree_vram_gb=$FREE_VRAM_GB -vlayer_per_gb=$layer_per_gb "BEGIN{printf(\"%d\n\",int(free_vram_gb*layer_per_gb))}")
        fi
    fi

    if [ "${DEBUG}" ];
    then
        log_debug "FREE_VRAM_GB=${FREE_VRAM_GB} MAX_NGL_EST=${MAX_NGL_EST} GPU=${GPU}"
    fi
}

function truncate_to_context_length {
    # todo: get better estimate of prompt length
    # assume 4 chars per token
    PROMPT_LENGTH_EST=$((${#PROMPT}/4))

    if [ "${PROMPT_LENGTH_EST}" -gt "${CONTEXT_LENGTH}" ];
    then
	log_warn "* prompt len ${PROMPT_LENGTH_EST} estimated not to fit in context ${CONTEXT_LENGTH}"
    fi

    if [ "$CONTEXT_LENGTH" -gt "$MAX_CONTEXT_LENGTH" ];
    then
	CONTEXT_LENGTH="$MAX_CONTEXT_LENGTH"
	log_warn "* truncated context length to $CONTEXT_LENGTH"
    fi
}

# if [ -n "${VIA_CLI_FUNCTIONS_LOADED}" ]; then
#     log_and_exit "VIA_CLI_FUNCTIONS_LOADED again"
# else
#     VIA_CLI_FUNCTIONS_LOADED=1
#     log_error "VIA_CLI_FUNCTIONS_LOADED first_time"
# fi

