#!/bin/bash

SCRIPT_DIR="$(dirname "$(realpath "${BASH_SOURCE}")")"

USAGE="[--via api] | [ --via cli [-m|--model-type model-type] [--fast | --long] [--context-length|-c n] [--ngl n] [--n-predict n] ]
       [--stdin|--interactive|-i] [--temperature temp] [--n-predict n] [--grammar-file file.gbnf] [--info] [--verbose|-v] [--debug] [--] QUESTION*"

### If there are any args, require "--" or any non-hyphen word to terminate args and start question.
### Assume the whole args is a question if there is no hyphen to start.
### There may be no question, if all is contained in SYSTEM_MESSAGE and STDIN.
INPUT=""
: "${QUESTION:=}"
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
                --via) shift; VIA="$1" ;;
                --model-type|-m) shift; MODEL_TYPE="$1" ;;
		# Logging 
                --info) INFO=1 ;;
                --verbose|-v) VERBOSE=1 ;;
                --debug) ERROR_OUTPUT="/dev/stdout"; SILENT_PROMPT=""; DEBUG=1; LOG_DISABLE=" " ;; # LOG_DISABLE space requried to override default
                --noerror) ERROR_OUTPUT="/dev/null" ;;
		# Generation options
                --n-predict) shift; N_PREDICT="$1" ;;
                --temperature) shift; TEMPERATURE="$1" ;;
                --grammar-file) shift; GRAMMAR_FILE="$1" ;;
		# Input Options
		--stdin|--interactive|-i) DO_STDIN=1 ;;
                --raw-input) RAW_FLAG="--raw-input" ;;
                --process-question-escapes|-e) PROCESS_QUESTION_ESCAPES=1 ;;
		# todo: these next four options only apply to --cli models so move them to via/cli
                --fast) PRIORITY="speed" ;;
                --long) PRIORITY="length" ;;
                --context-length|-c) shift; CONTEXT_LENGTH="$1" ;;
                --ngl) shift; NGL="$1" ;;
		# prompt
                --) shift; QUESTION=("$*"); break ;; # consumes rest of line
                -*) echo "Unrecognized option: $1" >> /dev/stderr; exit 1 ;;
                *) QUESTION=("$*"); break ;; # consumes rest of line
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
### Process flags and environment variables
###
: "${VIA:=cli}"
: "${MODEL_TYPE:=mistral}"
# Logging
: "${INFO:=${VERBOSE}}"
: "${VERBOSE:=}"
: "${DEBUG:=}"
: "${LOG_DISABLE:=--log-disable}" # use space ' ' to override
: "${KEEP_PROMPT_TEMP_FILE:=ALL}" # "NONE"|"ERROR"|"ALL"
# Old versions of llamafile sometimes need -silent-prompt or --no-display-prompt
# edit this, or use FORCE_MODEL_RUNNER and a newer MODEL_RUNNER .
: "${SILENT_PROMPT:=--silent-prompt --no-display-prompt}"
# Geneation Options
: "${TEMPERATURE:=}"
: "${N_PREDICT:=}"
: "${SYSTEM_MESSAGE:=}" # used to default to "Answer the following user question:"
: "${GPU:=auto}"        # auto|nvidia|omit
: "${GRAMMAR_FILE:=}"
: "${BATCH_SIZE:=}"
: "${SEED:=-1}"
# Extra
: "${LLM_ADDITIONAL_ARGS:=}"
# todo: move these to CLI module
: "${FORCE_MODEL_RUNNER:=}"
: "${LLAMAFILE_MODEL_RUNNER:="$(realpath "${SCRIPT_DIR}/../lib/llamafile-0.6.2") -m"}"
: "${NGL:=}"
: "${CONTEXT_LENGTH:=}"
: "${PRIORITY:=manual}" # speed|length|manual


###
### These variables are not settable by environment variables
###
PROCESS_QUESTION_ESCAPES=""
MODEL_RUNNER="/usr/bin/env"
RAW_FLAG=""
ERROR_OUTPUT="/dev/null"
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
function process_stdin() {
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
process_stdin
prepare_model
log_info "MODEL_PATH=${MODEL_PATH}"
adjust_raw_flag
truncate_to_context_length
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
