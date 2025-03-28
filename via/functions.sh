#!/bin/bash

# force umask - any tmp files and any files we create inadvertently should be private
umask 077

# Assume we are called from llamafiles script directory
# todo: fix the many global variable dependencies in this file
VIA_DIRECTORY="$(realpath "${SCRIPT_DIR}/../via")"
VIA_CLI_FUNCTIONS_PATH="$(realpath "${VIA_DIRECTORY}/cli/functions.sh")"
VIA_API_FUNCTIONS_PATH="$(realpath "${VIA_DIRECTORY}/api/functions.sh")"
MODELS_DIRECTORY="$(realpath "${SCRIPT_DIR}/../models")"
TMPFILES_TO_DELETE=""
COLOR_RED='\033[1;31m'
COLOR_YELLOW='\033[1;33m'
COLOR_GREEN='\033[1;32m'
COLOR_BLUE='\033[1;36m'
NOCOLOR='\033[0m'

# RWK: https://gist.github.com/akostadinov/33bb2606afe1b334169dfbf202991d36?permalink_comment_id=4962266#gistcomment-4962266
function stack_trace() {
    local status_code="${1}"
    local -a stack=("Stack trace of error code '${status_code}':")
    local stack_size=${#FUNCNAME[@]}
    local -i i
    local indent="    "
    # to avoid noise we start with 1 to skip the stack function
    for (( i = 1; i < stack_size; i++ )); do
	local func="${FUNCNAME[$i]:-(top level)}"
	local -i line="${BASH_LINENO[$(( i - 1 ))]}"
	local src="${BASH_SOURCE[$i]:-(no file)}"
	stack+=("$indent └ $src:$line ($func)")
	indent="${indent}    "
    done
    (IFS=$'\n'; echo "${stack[*]}")
}

function log_verbose {
    local prog="$(basename "$0")"
    local message="$1"
    if [ -n "${VERBOSE}" ];
    then
	printf "📣 %s: %s\n" "${prog}" "${message}" > /dev/stderr
    fi
}

function log_debug {
    local prog="$(basename "$0")"
    local message="$1"
    if [ -n "${DEBUG}" ]; then
	   printf "🐞 ${COLOR_BLUE}%s:${NOCOLOR} %s\n" "${prog}" "${message}" > /dev/stderr
    fi
}

function log_info {
    local prog="$(basename "$0")"
    local message="$1"
    if [ -n "${INFO}" ]; then
	printf "✅ ${COLOR_GREEN}INFO %s:${NOCOLOR} %s\n" "${prog}" "${message}" > /dev/stderr
    fi
}

function log_warn {
    local prog="$(basename "$0")"
    local code=$1
    local message="$2"
    printf "⚠️  ${COLOR_YELLOW}WARN %s (%s):${NOCOLOR} %s\n" "${prog}" "${code}" "${message}" > /dev/stderr
}

function log_error {
    local prog="$(basename "$0")"
    local message="$1"
    local code=$?
    printf "❌ ${COLOR_RED}ERROR in %s:${NOCOLOR} %s\n" "${prog}" "${message}" > /dev/stderr
    [ -n "${PRINT_STACK_TRACE}" ] && printf "%s\n" "$(stack_trace $code)" > /dev/stderr
}

function log_and_exit {
    local prog="$(basename "$0")"
    local code="$1"
    local message="$2"
    printf "⛔ ${COLOR_RED}ERROR in %s:${NOCOLOR} %s\n" "${prog}" "${message}" > /dev/stderr
    [ -n "${PRINT_STACK_TRACE}" ] && printf "%s\n" "$(stack_trace "$code")" > /dev/stderr
    [[ "${code}" =~ ^[0-9]+$ ]] && exit "${code}" || exit 1
}

# Check if the script is being sourced or directly executed
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    log_and_exit 1 "This script is intended to be sourced, not executed directly."
fi

function source_functions {
    local functions_path="$1"
    if [[ "${functions_path}" == "" ]]; then
	log_and_exit 3 "* $0: ERROR: source_functions path is empty"
    elif [[ -f "${functions_path}" ]]; then
	log_verbose "* sourcing ${functions_path}"
	[ "${VERBOSE}" ] && [ -n "${PRINT_STACK_TRACE}" ] && log_verbose "* $0: stack trace:\n%s\n" "$(stack_trace $code)"
	source "${functions_path}"
    elif [[ -z "${functions_path}" ]]; then
	log_and_exit 3 "* $0: ERROR: functions_path is empty"
    else
	log_and_exit 3 "* $0: ERROR: Cannot find functions: ${functions_path}"
    fi
}

k=0

# caller should add result to $TMPFILES_TO_DELETE
function mktemp_file() {
    local prefix="$1"
    prefix="${prefix:-llm}"

    local fn="$(mktemp -t "${prefix}.XXXXXX")"
    local status=$?

    if [ $status -ne 0 ]; then
        log_and_exit 4 "Cannot make temp file with prefix=${prefix}: Status=${status}"
    fi

    printf "%s\n" "${fn}"
}

TMPFILES_TO_DELETE=""

function register_temp_file() {
    local fn="$1"
    TMPFILES_TO_DELETE="${fn} ${TMPFILES_TO_DELETE}"
}

function cleanup_temp_files() {
    local status="$1"

    case "${KEEP_PROMPT_TEMP_FILE}" in
        ALL)
            true
            ;;
        ERROR|ERRORS)
            if [ "$status" -ne 0 ]; then
                log_warn "Leaving temporary files: ${TMPFILES_TO_DELETE}"
            elif [ -n "${TMPFILES_TO_DELETE}" ]; then
                log_info "Cleaning up temporary files: ${TMPFILES_TO_DELETE}"
                for file in ${TMPFILES_TO_DELETE}; do
                    cleanup_file "$file"
                done
            fi
            ;;
        NONE)
            if [ -n "${TMPFILES_TO_DELETE}" ]; then
                log_info "Cleaning up temporary files: ${TMPFILES_TO_DELETE}"
                for file in ${TMPFILES_TO_DELETE}; do
                    cleanup_file "$file"
                done
            fi
            ;;
        *)
            log_warn "Invalid value for KEEP_PROMPT_TEMP_FILE: ${KEEP_PROMPT_TEMP_FILE}.  No cleanup performed."
            ;;
    esac
}

function cleanup_file() {
    local file="$1"

    if [ -d "${file}" ]; then
        log_warn "cleanup_file: skipping dir ${file}"
        return 1
    fi

    if [ "${file}" == "/dev/null" ]; then
        log_warn "cleanup_file: skipping /dev/null: ${file}"
        return 1
    fi

    if [ "${file}" == "/tmp?" ]; then
        log_warn "cleanup_file: skipping /tmp: ${file}"
    fi

    if [ -n "${file}" ]; then
        if ! rm -f "${file}" ; then
            log_error "Error Removing ${file}" $?
        fi
    fi
}

function init_model {
    if [ "${VIA}" == "api" ];
    then
	source_functions "${VIA_API_FUNCTIONS_PATH}"
    elif [ "${VIA}" == "cli" ];
    then
	source_functions "${VIA_CLI_FUNCTIONS_PATH}"
    else
	log_and_exit 3 "Unknown VIA=${VIA}"
    fi
    init_via_model
}

# if [ -n "${VIA_FUNCTIONS_LOADED}" ]; then
#     log_error_and_exit "VIA_FUNCTIONS_LOADED again"
# else
#     VIA_FUNCTIONS_LOADED=1
#     log_error "VIA_FUNCTIONS_LOADED first_time"
# fi
