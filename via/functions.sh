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

# Check if the script is being sourced or directly executed
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    log_and_exit 1 "This script is intended to be sourced, not executed directly."
fi

source "${VIA_DIRECTORY}/logging.sh"

function source_functions {
    local functions_path="$1"
    if [ -z "${functions_path}" ]; then
	log_and_exit 3 "* $0: ERROR: source_functions path is empty"
    elif [ -f "${functions_path}" ]; then
	log_verbose "* sourcing ${functions_path}"
	[ -n "${VERBOSE}" ] && [ -n "${PRINT_STACK_TRACE}" ] && log_verbose "* $0: stack trace:\n%s\n" "$(stack_trace $code)"
	source "${functions_path}"
    elif [ -z "${functions_path}" ]; then
	log_and_exit 3 "* $0: ERROR: functions_path is empty"
    else
	log_and_exit 3 "* $0: ERROR: Cannot find functions: ${functions_path}"
    fi
}

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
            log_warn "Leaving temporary files: ${TMPFILES_TO_DELETE}"
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
