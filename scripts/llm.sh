#!/bin/bash

SCRIPT_DIR="$(dirname "$(realpath "${BASH_SOURCE}")")"

USAGE="[-m|--model-type model-type] [--stdin|--interactive|-i] [--fast | --long] [--temperature temp] [--context-length|-c n] [--ngl n] [--n-predict n] [--debug] [--verbose|-v] [--grammar-file file.gbnf] [--] QUESTION*"

###
### CLI arg parsing
###
### If there are any args, require "--" or any non-hyphen word to terminate args and start question.
### Assume the whole args is a question if there is no hyphen to start.
### There may be no question, if all is contained in SYSTEM_MESSAGE and STDIN.
INPUT=""
QUESTION="${QUESTION:-}"
DO_STDIN="$(test -t 0 || echo $?)"

function parse_args() {
    if [[ "${1}" == "-"* ]];
    then
	while [[ $# -gt 0 ]]; do
            case $1 in
		--help)
		    printf "$0: %s\n" "${USAGE}" >> /dev/stderr
		    exit 0
		    ;;
		--via)
		    shift; VIA="$1" ;;
		-m|--model-type)
                    shift; MODEL_TYPE="$1" ;;
		--fast)
                    PRIORITY="speed" ;;
		--long)
                    PRIORITY="length" ;;
		--temperature)
                    shift; TEMPERATURE="$1" ;;
		--verbose|-v)
                    VERBOSE=1 ;;
		--info)
                    INFO=1 ;;
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
                    shift; QUESTION=("$*")
                    break
                    ;;
		-*)
                    log_and_exit 1 "Unrecognized option: $1"
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
	QUESTION=("$*")
    fi
}

parse_args ${@}

###
### Site Variables
### Set site variables from env.sh
### 
[ -z "${IN_LLM_SH_ENV}" ] && [ -f "${SCRIPT_DIR}/env.sh" ] && source "${SCRIPT_DIR}/env.sh"

###
### Process CLI flags and environment variables
###
VIA=${VIA:-cli}
MODEL_TYPE=${MODEL_TYPE:-mistral}
TEMPERATURE=${TEMPERATURE:-}
CONTEXT_LENGTH=${CONTEXT_LENGTH:-}
N_PREDICT="${N_PREDICT:-}"
SYSTEM_MESSAGE="${SYSTEM_MESSAGE-"Answer the following user question:"}"
NGL="${NGL:-}"
GPU="${GPU:-auto}"		# auto|nvidia|omit
PRIORITY="${PRIORITY:-manual}" # speed|length|manual
DEBUG="${DEBUG:-}"
VERBOSE="${VERBOSE:-}"
INFO="${INFO:-${VERBOSE}}"
LOG_DISABLE="--log-disable"
GRAMMAR_FILE="${GRAMMAR_FILE:-}"
BATCH_SIZE="${BATCH_SIZE:-}"
SEED="${SEED:--1}"
LLAMAFILE_MODEL_RUNNER="${LLAMAFILE_MODEL_RUNNER:-"$(realpath "${SCRIPT_DIR}/../lib/llamafile-0.6.2") -m"}"
FORCE_MODEL_RUNNER="${FORCE_MODEL_RUNNER:-}"
LLM_ADDITIONAL_ARGS="${LLM_ADDITIONAL_ARGS:-}"
KEEP_PROMPT_TEMP_FILE="${KEEP_PROMPT_TEMP_FILE:-ALL}" # "NONE"|"ERROR"|"ALL"

###
### These variables are not settable via environment
###
PROCESS_QUESTION_ESCAPES=""
MODEL_RUNNER="/usr/bin/env"
RAW_FLAG=""
ERROR_OUTPUT="/dev/null"
# Old versions of llamafile sometimes need -silent-prompt or --no-display-prompt
# edit this, or use FORCE_MODEL_RUNNER and a newer MODEL_RUNNER .
SILENT_PROMPT="${SILENT_PROMPT:---silent-prompt --no-display-prompt}"
# NO_PENALIZE_NL is gone and we only have --penalize-ml in llamafile 0.7
#NO_PENALIZE_NL="--no-penalize-nl "
NO_PENALIZE_NL=""


###
### Load functions for API or CLI
### 
VIA_DIRECTORY="$(realpath "${SCRIPT_DIR}/../via")"
FUNCTIONS_PATH="$(realpath "${VIA_DIRECTORY}/functions.sh")"
VIA_CLI_FUNCTIONS_PATH="$(realpath "${VIA_DIRECTORY}/cli/functions.sh")"
VIA_API_FUNCTIONS_PATH="$(realpath "${VIA_DIRECTORY}/api/functions.sh")"
source "${FUNCTIONS_PATH}"

###
### Prompt and STDIN processing

# todo: move this to where it is used
PROMPT_TEMP_FILE="$(mktemp_file "prompt")"

# Process escape sequences in QUESTION if requested.
# This will turn literal "\n" into literal newline in the QUESTION>
# STDIN is never processed for escapes.
function process_question_escapes() {
    if [ "${PROCESS_QUESTION_ESCAPES}" ]; then
	log_verbose "Processing escape sequences in QUESTION"
	printf -v QUESTION "%b" "$QUESTION"
    fi
}

# Read STDIN into INPUT, and prompt if stdin is tty
function do_stdin() {
    if [ -n "$DO_STDIN" ];
    then
	if [ -t 0 ];
	then
            echo "Give input followed by Ctrl-D:"
	fi
	INPUT=$(cat)
    fi
}

###
### Debug and log
###

function set_verbose_debug {
    # Set verbose and debug last
    if [ "${DEBUG}" ] || [ "${INFO}" ] || [ "${VERBOSE}" ];
    then
	log_info "Parameters: ngl=${NGL} context_length=${CONTEXT_LENGTH} est_len=${PROMPT_LENGTH_EST}"
    fi
    if [ "${DEBUG}" ];
    then
	set -x
    fi
}

# Try to inform user about errors
function report_success_or_fail {
    status=$1
    if [ $status -ne 0 ];
    then
	if [ "${ERROR_OUTPUT}" == "/dev/null" ];
	then
	    log_error "FAIL STATUS=$status: re-run with --debug"
	else
	    log_error "FAIL STATUS=$status: errors went to ${ERROR_OUTPUT}" > /dev/stderr
	fi
    fi
    return $STATUS
}

# if --raw-input is specified, use stdin as the only text to send to the model
function adjust_raw_flag {
    if [ -n "${RAW_FLAG}" ]; then
	PROMPT="${INPUT}"
	SYSTEM_MESSAGE=""
    fi
}

###
### Perform Inference
###

function perform_inference() {
    case "${VIA}" in
        "api")
            via_set_options
            via_api_perform_inference "${MODEL_MODE}" "${SYSTEM_MESSAGE}" "${PROMPT}" "${GRAMMAR_FILE}" "${TEMPERATURE}" "${repeat_penalty}" "${penalize_nl}"
            status=$?
            ;;
        "cli")
            via_set_options
            cli_perform_inference
            status=$?
            ;;
        *)
            log_error "Unknown VIA=${VIA}"
            status=1
            ;;
    esac
    return $status
}

###
### Main Flow
###
init_model
process_question_escapes
do_stdin
prepare_model && [ -n "${INFO}" ] && log_info "MODEL_PATH=${MODEL_PATH}"
adjust_raw_flag
check_context_length
set_verbose_debug
perform_inference; STATUS=$?
report_success_or_fail $STATUS
cleanup_temp_files $STATUS
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
#       - `--fast` `--ngl`
#       AUTO MODE:
#       - proposal: `--fast` unless cmd input is given, then do `-context`
#         e.g. MIN_CONTEXT_LENGTH <= (input length * 2) <= MAX_CONTEXT_LENGTH???
#       MANUAL MODE:
#       - `--ngl` `--context-length`
