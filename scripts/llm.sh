#!/bin/bash

SCRIPT_DIR=$(dirname $(realpath "${BASH_SOURCE}"))

USAGE="[-m|--model-type model-type] [--stdin|--interactive|-i] [--speed | --length] [--temperature temp] [--context-length|-c n] [--ngl n] [--n-predict n] [--debug] [--verbose|-v][--] QUESTION*"

# Use CLI flags, or environment variables below:
MODEL_TYPE=${MODEL_TYPE:-mistral}
ERROR_OUTPUT="/dev/null"
TEMPERATURE=${TEMPERATURE:-}
CONTEXT_LENGTH=${CONTEXT_LENGTH:-}
N_PREDICT="${N_PREDICT:-}"
SYSTEM_MESSAGE="${SYSTEM_MESSAGE-"Answer the following user question:"}"
SILENT_PROMPT="--silent-prompt"
NGL="${NGL:-}"
GPU="${GPU:-auto}"		# auto|nvidia|omit
PRIORITY="${PRIORITY:-manual}" # speed|length|manual
DEBUG="${DEBUG:-}"
VERBOSE=${VERBOSE:-}
LOG_DISABLE="--log-disable"
GRAMMAR_FILE="${GRAMMAR_FILE:-}"
BATCH_SIZE="${BATCH_SIZE:-}"
LLAMAFILE_MODEL_RUNNER="${LLAMAFILE_MODEL_RUNNER:-"$(realpath ${SCRIPT_DIR}/../lib/llamafile-0.6.2) -m"}"
FORCE_MODEL_RUNNER="${FORCE_MODEL_RUNNER:-}"
LLM_ADDITIONAL_ARGS="${LLM_ADDITIONAL_ARGS:-}"

# Not settable via ENV
PROCESS_QUESTION_ESCAPES=""
MODEL_RUNNER="/usr/bin/env"
CLI_MODE="--cli"
RAW_FLAG=""

# Read input
INPUT=""
QUESTION=""
DO_STDIN="$(test -t 0 || echo $?)"

# prompt machinery
KEEP_PROMPT_TEMP_FILE="${KEEP_PROMPT_TEMP_FILE:-ALL}" # "NONE"|"ERROR"|"ALL"
PROMPT_TEMP_FILE="/tmp/prompt.$$"

function set_threads() {
    # Get thread count
    if [ "${THREADS}" == "" ];
    then
	THREADS=$( ( [ -f /proc/cpuinfo ] && grep '^cpu cores\s*:' /proc/cpuinfo | head -1 | awk '{print $4}' ))
	if [ "${THREADS}" == "" ];
	then
            THREADS=$(sysctl -n hw.ncpu 2>/dev/null || echo "${NUMBER_OF_PROCESSORS:-4}")
	fi
    fi
    THREADS="${THREADS:+-t $THREADS}"
}

function parse_args() {
    # If there are any args, require "--" or any non-hyphen word to terminate args and start question.
    # Assume the whole args is a question if there is no hyphen to start.
    if [[ "${1}" == "-"* ]];
    then
	while [[ $# -gt 0 ]]; do
            case $1 in
		-m|--model-type)
                    shift; MODEL_TYPE="$1" ;;
		--speed)
                    PRIORITY="speed" ;;
		--length)
                    PRIORITY="length" ;;
		--temperature)
                    shift; TEMPERATURE="$1" ;;
		--verbose|-v)
                    VERBOSE=1 ;;
		-c|--context-length)
                    shift; CONTEXT_LENGTH="$1" ;;
		--ngl)
                    shift; NGL="$1" ;;
		--n-predict)
                    shift; N_PREDICT="$1" ;;
		--grammar-file)
                    shift; GRAMMAR_FILE="$1" ;;
		--debug)
                    ERROR_OUTPUT="/dev/stdout"; SILENT_PROMPT=""; DEBUG=1; LOG_DISABLE="" ;;
		--noerror)
                    ERROR_OUTPUT="/dev/null" ;;
		--stdin|--interactive|-i)
                    DO_STDIN=1 ;;
		--raw-input)
		    RAW_FLAG="--raw-input"
		    ;;
		-e|--process-question-escapes)
		    PROCESS_QUESTION_ESCAPES=1 ;;
		--)
                    # consumes rest of line
                    shift; QUESTION=("$@")
                    break
                    ;;
		-*)
                    echo "Unrecognized option: $1" >> /dev/stderr
                    exit 1
                    ;;
		*)
                    # consumes rest of line
                    QUESTION=("$*")
                    break
                    ;;
            esac
            shift
	done
    else
	QUESTION="${*}"
    fi
}

function process_question_escapes() {
    # Process escape sequences in QUESTION if requested.
    # STDIN never processes escapes.
    if [ "${PROCESS_QUESTION_ESCAPES}" ]; then
	[ $VERBOSE ] && echo "* Processing escape sequences in QUESTION"
	printf -v QUESTION "%b" "$QUESTION"
    fi
}

function do_stdin() {
    if [ "$DO_STDIN" != "" ];
    then
	if [ -t 0 ];
	then
            echo "Give input followed by Ctrl-D:"
	fi
	INPUT=$(cat)
    fi
}

function gpu_check {
    local layer_per_gb=("$@")

    if [ "${layer_per_gb}" == "" ];
    then
        layer_per_gb=1
    fi
    if [ "${GPU}" == "none" ] || ! gpu_detector=$(command -v nvidia-detector) || [[ "$($gpu_detector)" == "None" ]];
    then
        if [ "${DEBUG}" ];
        then
            echo "* NO GPU"
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
        echo "* FREE_VRAM_GB=${FREE_VRAM_GB} MAX_NGL_EST=${MAX_NGL_EST} GPU=${GPU}"
    fi
}

function cap_ngl {
    if [ "$GPU" != "none" ] && [ "$GPU" != "" ] && [ "${NGL}" != "" ] && [ "${NGL}" -gt "${MAX_NGL_EST}" ];
    then
        [ $VERBOSE ] && echo "* Capping $NGL at $MAX_NGL_EST"
        NGL=$MAX_NGL_EST
    fi
}

function load_model {
    if [ "${MODEL_TYPE}" == "" ];
    then
	echo "Model not found: ${MODEL_TYPE}" >> /dev/stderr
	exit 3
    fi

    # Construct the path to the functions file
    MODELS_DIRECTORY="$(realpath "${SCRIPT_DIR}/../models")"
    FUNCTIONS_PATH="$(realpath "${MODELS_DIRECTORY}/functions.sh")"
    MODEL_FUNCTIONS_PATH="$(realpath "${MODELS_DIRECTORY}/${MODEL_TYPE}/functions.sh")"

    # Check if the functions file exists
    if [[ -f "${FUNCTIONS_PATH}" ]]; then
	source "${FUNCTIONS_PATH}"
    else
	echo "* ERROR: Cannot find functions: ${FUNCTIONS_PATH}"
	exit 3
    fi

    # Check if the model functions file exists
    if [[ -f "${MODEL_FUNCTIONS_PATH}" ]]; then
	source "${MODEL_FUNCTIONS_PATH}"
    else
	echo "* ERROR: Cannot find model functions for ${MODEL_TYPE}: ${MODEL_FUNCTIONS_PATH}"
	exit 1
    fi

}

# todo: much work here
# for example, why not just call these all the same name?
function prepare_model {
    case "${MODEL_TYPE}" in
	mixtral) mixtral_model ;;
	nous-hermes) nous_hermes_model ;;
	dolphin) dolphin_model ;;
	mistral) mistral_model ;;
	codebooga) codebooga_model ;;
	cerebrum) cerebrum_model ;;
	deepseek-coder) deepseek_coder_model ;;
	rocket) rocket_model ;;
	phi) phi_model ;;
	via-api) via_api_model ;;
	*)
            echo "unknown model type $MODEL_TYPE" >> /dev/stderr
            exit 1
            ;;
    esac

    # if --raw-input is specified, use stdin as the only text to send to the model
    if [ "${RAW_FLAG}" != "" ]; then
	PROMPT="${INPUT}"
	SYSTEM_MESSAGE=""
    fi
}

function check_context_length {
    # memory allocation: assume 4 chars per token
    #PROMPT_LENGTH_EST=$(((75+${#SYSTEM_MESSAGE}+${#QUESTION}+${#INPUT})/4))
    PROMPT_LENGTH_EST=$((${#PROMPT}/4))

    if [ "$MODEL_TYPE" == "via-api" ];
    then
	return;
    fi

    if [ "${PROMPT_LENGTH_EST}" -gt "${CONTEXT_LENGTH}" ];
    then
	echo "* ERROR: Prompt len ${PROMPT_LENGTH_EST} estimated not to fit in context ${CONTEXT_LENGTH}"
	exit 2
    fi

    if [ "$CONTEXT_LENGTH" -gt "$MAX_CONTEXT_LENGTH" ];
    then
	CONTEXT_LENGTH="$MAX_CONTEXT_LENGTH"
	echo "* Truncated context length to $CONTEXT_LENGTH"
    fi

    #BATCH_SIZE=${BATCH_SIZE:-$(($CONTEXT_LENGTH / 2))}
}

function set_cli_options {
    # Calculate $LLM_SH command line options
    # Use bash :+ syntax to avoid setting prefixes on empty values
    N_PREDICT="${N_PREDICT:+--n-predict $N_PREDICT}"
    TEMPERATURE="${TEMPERATURE:+--temp $TEMPERATURE}"
    CONTEXT_LENGTH="${CONTEXT_LENGTH:+-c $CONTEXT_LENGTH}"
    BATCH_SIZE="${BATCH_SIZE:+--batch_size $BATCH_SIZE}"
    NGL="${NGL:+-ngl $NGL}"
    GPU="${GPU:+--gpu $GPU}"
}

function set_model_runner {
    # set MODEL_RUNNER
    if [ "${MODEL##*.}" == "gguf" ] || [ "${FORCE_MODEL_RUNNER}" ];
    then
	MODEL_RUNNER="${LLAMAFILE_MODEL_RUNNER}"
    fi
}

function set_verbose_debug {
    # Set verbose and debug last
    if [ "${DEBUG}" ] || [ "${VERBOSE}" ];
    then
	printf '* Parameters: ngl=%s context_length=%s est_len=%s:\n' "${NGL}" "${CONTEXT_LENGTH}" "${PROMPT_LENGTH_EST}"
	set -x
    fi
}

function fixup_input {
    # workaround for https://github.com/Mozilla-Ocho/llamafile/issues/288
    sed -e 's/<img src="/<img  src="/g'
}

function cli_perform_inference {
    # Use llamafile or similar CLI runner to perform inference
    # return status
    # set -x
    printf '%s' "${PROMPT}" > "${PROMPT_TEMP_FILE}"
    if [ "${GRAMMAR_FILE}" != '' ]; then
	GRAMMAR_FILE="--grammar-file ${GRAMMAR_FILE}"
    fi
    cat "${PROMPT_TEMP_FILE}" | fixup_input | ${MODEL_RUNNER} ${MODEL} ${CLI_MODE} ${LOG_DISABLE} ${GPU} ${NGL} ${GRAMMAR_FILE} ${TEMPERATURE} ${CONTEXT_LENGTH} ${N_PREDICT} ${BATCH_SIZE} --no-penalize-nl --repeat-penalty 1 ${THREADS} -f /dev/stdin $SILENT_PROMPT ${LLM_ADDITIONAL_ARGS} 2> "${ERROR_OUTPUT}"
    return $?
}

# Try to inform user about errors
function report_success_or_fail {
    status=$1
    if [ $status -ne 0 ];
    then
	if [ "${ERROR_OUTPUT}" == "/dev/null" ];
	then
	    echo "* FAIL STATUS=$status: re-run with --debug" > /dev/stderr
	else
	    echo "* FAIL STATUS=$status: errors went to ${ERROR_OUTPUT}" > /dev/stderr
	fi
    fi
    return $STATUS
}

function handle_temp_files {
    status=$1
    if [ -f "${PROMPT_TEMP_FILE}" ];
    then
	case "$KEEP_PROMPT_TEMP_FILE" in
	    ALL)
		true
		;;
	    ERROR|ERRORS)
		if [ $status -ne 0 ];
		then
		    echo "* PROMPT=${PROMPT_TEMP_FILE}" > /dev/stderr
		else
		    rm "${PROMPT_TEMP_FILE}"
		fi
		;;
	    NONE)
		rm "${PROMPT_TEMP_FILE}"
		;;
	esac
    else
	if [ "$KEEP_PROMPT_TEMP_FILE" == "" ] && [ -f "${PROMPT_TEMP_FILE}" ]; 
	then
	    rm "${PROMPT_TEMP_FILE}"
	fi
    fi
}

set_threads
parse_args "$@"
load_model
process_question_escapes
do_stdin
prepare_model
check_context_length
if [ "$MODEL_TYPE" != "via-api" ];
then
   # fixme
   set_cli_options
fi
set_model_runner
set_verbose_debug
if [ "$MODEL_TYPE" == "via-api" ];
then
    repeat_penalty="1"
    penalize_nl="false"
    via_api_perform_inference "instruct" "${SYSTEM_MESSAGE}" "${PROMPT}" "${GRAMMAR_FILE}" "${TEMPERATURE}" "${repeat_penalty}" "${penalize_nl}"
    # fixme add these parameters if they make sense
    # ${TEMPERATURE} ${N_PREDICT} ${BATCH_SIZE} ${LLM_ADDITIONAL_ARGS}
else
    cli_perform_inference
fi
STATUS=$?
report_success_or_fail $STATUS
handle_temp_files $STATUS
exit $STATUS

###
### TODO
###
# TODO: add few-shot to supplement system message, since at least in chatml each goes in as an assistant turn
# TODO: bash parsing of CLI parameters vs ENV vs bundles of settings is a mess
# TODO: calculate context and GPU layers based on input, output, existing memory, and user-preference
#       by analogy to "CAMERA MODE" aperture priority, shutter priority, auto, or manual
#       context priority, speed priority, auto, or manual
#       CONTEXT PRIORITY MODE:
#       - could be used for input or output
#         for input, assume some k * (prompt len + input len)
#         for output, probably need an explicit declaration we want a long output, hard to detect otherwise
#       SPEED PRIORITY MODE:
#       - `--speed` `--ngl`
#       AUTO MODE:
#       - proposal: `--speed` unless cmd input is given, then do `-context`
#         e.g. MIN_CONTEXT_LENGTH <= (input length * 2) <= MAX_CONTEXT_LENGTH???
#       MANUAL MODE:
#       - `--ngl` `--context-length`
