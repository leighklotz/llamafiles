#!/bin/bash

# Assume we are called from llamafiles script directory
# todo: fix the many global variable dependencies in this file
VIA_DIRECTORY="$(realpath "${SCRIPT_DIR}/../via")"
VIA_CLI_FUNCTIONS_PATH="$(realpath "${VIA_DIRECTORY}/cli/functions.sh")"
VIA_API_FUNCTIONS_PATH="$(realpath "${VIA_DIRECTORY}/api/functions.sh")"
MODELS_DIRECTORY="$(realpath "${SCRIPT_DIR}/../models")"

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
    printf "* ERROR in %s: %s\n%s\n" "${prog}" "${message}" "$(stack_trace $?)" > /dev/stderr
}

function log_and_exit {
    local prog="$(basename "$0")"
    local code=$1
    local message="$2"
    printf "* ERROR in %s: %s\n%s\n" "${prog}" "${code}" "${message}" "$(stack_trace "${code}")" > /dev/stderr
    [[ $code =~ ^[0-9]+$ ]] && exit $code || exit 1
}

# Check if the script is being sourced or directly executed
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    log_and_exit 1 "This script is intended to be sourced, not executed directly."
fi

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
