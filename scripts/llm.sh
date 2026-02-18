#!/bin/bash

SCRIPT_DIR="$(dirname "$(realpath "${BASH_SOURCE}")")"

USAGE='[--stdin|--interactive|-i] [--raw-input] \
[--process-question-escapes|-e] [--temperature temp] [--n-predict n] \
[--grammar-file file.gbnf] [--info] [--verbose|-v] [--debug] [--noerror] [--] QUESTION*'

function usage {
    printf "Usage: %s %s\n" "$0" "${USAGE}" >> /dev/stderr
}

### If there are any args, require “--” or any non‑hyphen word to terminate args
### and start question.  Assume the whole args is a question if there is no hyphen.
INPUT=""
: "${QUESTION:=}"
DO_STDIN="$(test -t 0 || echo $?)"

function parse_args() {
    if [[ "${1}" == "-"* ]]; then
        while [[ $# -gt 0 ]]; do
            case $1 in
                --help)
                    usage
                    exit 0
                    ;;
                # --via removed
                --inference-mode) shift; INFERENCE_MODE="$1" ;;
                # Logging
                --info) INFO=1 ;;
                --verbose|-v) VERBOSE=1 ;;
                --debug) # LOG_DISABLE=" " to override default
                    ERROR_OUTPUT="/dev/stdout"; DEBUG=1; LOG_DISABLE=" "
                    ;;
                --noerror) ERROR_OUTPUT="/dev/null" ;;
                # Generation options
                --n-predict) shift; N_PREDICT="$1" ;;
                --temperature) shift; TEMPERATURE="$1" ;;
                --grammar-file) shift; GRAMMAR_FILE="$1" ;;
                # Input options
                --stdin|--interactive|-i) DO_STDIN=1 ;;
                --raw-input) RAW_FLAG="--raw-input" ;;
                --process-question-escapes|-e) PROCESS_QUESTION_ESCAPES=1 ;;
                --) shift; QUESTION=("$*"); break ;;  # consumes rest of line
                -*) printf "Unrecognized option: %s\n\n" "$1" >> /dev/stderr
                    usage
                    exit 1
                    ;;
                *) QUESTION=("$*"); break ;;  # consumes rest of line
            esac
            shift
        done
    else
        QUESTION=("$*")
    fi
}

parse_args "${@}"

##############################################################################
#  Site variables – set from env.sh if present
##############################################################################
[ -z "${IN_LLM_SH_ENV}" ] && [ -f "${SCRIPT_DIR}/env.sh" ] && source "${SCRIPT_DIR}/env.sh"

##############################################################################
#  Process flags and environment variables
##############################################################################
# Logging options
: "${INFO:=${VERBOSE}}"
: "${VERBOSE:=}"
: "${DEBUG:=}"
: "${LOG_DISABLE:=--log-disable}"   # use space ' ' to override
: "${KEEP_PROMPT_TEMP_FILE:=ALL}" # "NONE"|"ERROR"|"ALL"
# Generation options
: "${INFERENCE_MODE:=instruct}"
: "${TEMPERATURE:=}"
: "${N_PREDICT:=}"
: "${SYSTEM_MESSAGE:=}"
: "${USE_GRAMMAR:=}"
: "${GRAMMAR_FILE:=}"
: "${SEED:=NaN}"
: "${REPEAT_PENALTY:=1}"
: "${PENALIZE_NL:=false}"
: "${USE_SYSTEM_ROLE:=}"
: "${REASONING_EFFORT:=low}"

##############################################################################
#  Load shared functions
##############################################################################
FUNCTIONS_PATH="$(realpath "${SCRIPT_DIR}/../via/functions.sh")"
VIA_API_FUNCTIONS_PATH="$(realpath "${SCRIPT_DIR}/../via/api/functions.sh")"
source "${FUNCTIONS_PATH}"

##############################################################################
#  Perform Inference
##############################################################################
function perform_inference {
    via_api_perform_inference "${INFERENCE_MODE}" "${SYSTEM_MESSAGE}" \
        "${PROMPT}" "${GRAMMAR_FILE}" "${TEMPERATURE}" "${REPEAT_PENALTY}" "${PENALIZE_NL}" "${N_PREDICT}"
    status=$?
    return $status
}

# Process escape sequences in QUESTION if requested.
# This will turn literal "\n" into literal newline in the QUESTION>
# STDIN is never processed for escapes.
function process_question_escapes() {
    if [ -n "${PROCESS_QUESTION_ESCAPES}" ]; then
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
        INPUT="$(cat | tr '\0' ' ')"
    fi
}


##############################################################################
### Debug and log
##############################################################################
function set_verbose_debug {
    if [ -n "${DEBUG}" ];
    then
        set -x
    fi
}

# Try to inform user about errors
function report_success_or_fail {
    local status="$1"
    if [ $status -ne 0 ];
    then
        if [ "${ERROR_OUTPUT}" == "/dev/null" ];
        then
            log_error "FAIL STATUS=$status: re-run with --debug"
        else
            log_error "FAIL STATUS=$status: errors went to ${ERROR_OUTPUT}" > /dev/stderr
        fi
    fi
    return $status
}

##############################################################################
#  Main Flow
##############################################################################
source "${VIA_API_FUNCTIONS_PATH}"

process_question_escapes
log_info "process_stdin"
process_stdin
log_info "process_stdin done"
prepare_prompt
set_verbose_debug
log_info "perform_inference"
perform_inference; STATUS=$?
log_info "perform_inference done"
report_success_or_fail $STATUS
cleanup_temp_files $STATUS
exit $STATUS
