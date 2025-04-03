#!/bin/bash

# Check if the script is being sourced or directly executed
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    log_and_exit 1 "This script is intended to be sourced, not executed directly."
fi

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
