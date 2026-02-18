#!/bin/bash

# Check if the script is being sourced or directly executed
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    echo "This script is intended to be sourced, not executed directly." >> /dev/stderr
    exit 1
fi

COLOR_RED='\e[1;31m'
COLOR_YELLOW='\e[1;33m'
COLOR_GREEN='\e[1;32m'
COLOR_BLUE='\e[1;36m'
NOCOLOR='\e[0m'

# RWK: https://gist.github.com/akostadinov/33bb2606afe1b334169dfbf202991d36?permalink_comment_id=4962266#gistcomment-4962266
function stack_trace() {
    local -a stack=("Stack trace:")
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

function log_with_icon {
    local icon="$1"
    local message="$2"
    local timestamp="$(date -u +"%Y-%m-%d %H:%M:%S.%3NZ")"
    printf "%s %s %b\n" "${icon}" "${timestamp}" "${message}" > /dev/stderr
}

function log_verbose {
    local prog="$(basename "$0")"
    local message="$1"
    if [ -n "${VERBOSE}" ]; then
        log_with_icon '📣' "${prog}: ${message}"
    fi
}

function log_debug {
    local prog="$(basename "$0")"
    local message="$1"
    if [ -n "${DEBUG}" ]; then
        log_with_icon '🐞' "${COLOR_BLUE}${prog}:${NOCOLOR} ${message}"
    fi
}

function log_info {
    local prog="$(basename "$0")"
    local message="$1"
    if [ -n "${INFO}" ]; then
        log_with_icon '✅' "${COLOR_GREEN}INFO ${prog}:${NOCOLOR} ${message}"
    fi
}

function log_warn {
    local prog="$(basename "$0")"
    local message="$1"
    log_with_icon '⚠️ ' "${COLOR_YELLOW}WARN ${prog}:${NOCOLOR} ${message}" # note there is another space and single quote hidden by the icon
}

function log_error {
    local prog="$(basename "$0")"
    local message="$1"
    log_with_icon '❌' "${COLOR_RED}ERROR in ${prog}:${NOCOLOR} ${message}"
    [ -n "${PRINT_STACK_TRACE}" ] && printf "%s\n" "$(stack_trace)" > /dev/stderr
}

function log_and_exit {
    local prog="$(basename "$0")"
    local code="$1"
    local message="$2"
    log_with_icon '⛔' "${COLOR_RED}ERROR in ${prog}:${NOCOLOR} ${message}"
    [ -n "${PRINT_STACK_TRACE}" ] && printf "Error code %s %s\n" "$code" "$(stack_trace)" | tee > /dev/stderr
    [[ "${code}" =~ ^[0-9]+$ ]] && exit "${code}" || exit 1
}
