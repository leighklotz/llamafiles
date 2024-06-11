#!/bin/bash

# Assume we are called from llamafiles script directory
# todo: fix the many global variable dependencies in this file
VIA_DIRECTORY="$(realpath "${SCRIPT_DIR}/../via")"
VIA_CLI_FUNCTIONS_PATH="$(realpath "${VIA_DIRECTORY}/cli/functions.sh")"
VIA_API_FUNCTIONS_PATH="$(realpath "${VIA_DIRECTORY}/api/functions.sh")"
MODELS_DIRECTORY="$(realpath "${SCRIPT_DIR}/../models")"

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

# Dup from llm.sh
# fixme: dup code from llm.sh
function source_functions {
    local functions_path="$1"
    if [[ -f "${functions_path}" ]]; then
	log_verbose "* sourcing ${functions_path}"
	source "${functions_path}"
    else
	log_and_exit 3 "* $0: ERROR: Cannot find functions: ${functions_path}"
    fi
}

function init_model {
    if [ "${MODEL_TYPE}" == "via-api" ];
    then
	source "${VIA_API_FUNCTIONS_PATH}"
    else
	source "${VIA_CLI_FUNCTIONS_PATH}"
    fi
    init_via_model
}
