#!/bin/bash

# force umask - any tmp files and any files we create inadvertently should be private
umask 077

# Assume we are called from llamafiles script directory
# todo: fix the many global variable dependencies in this file
VIA_DIRECTORY="$(realpath "${SCRIPT_DIR}/../via")"
source "${VIA_DIRECTORY}/logging.sh"
# Check if the script is being sourced or directly executed
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    log_and_exit  "This script is intended to be sourced, not executed directly." >> /dev/stderr
    exit 1
fi

declare -a TMPFILES_TO_DELETE=()

function register_temp_file {
    local fn=$1
    if [ -z "$fn" ]; then
        log_and_exit 4 "register_temp_file missing arg"
    fi
    TMPFILES_TO_DELETE+=("$fn")
}

function mktemp_file {
    local prefix="${1:-llm}"
    local fn
    fn="$(mktemp -t "${prefix}.XXXXXX")" || {
        log_and_exit 4 "Cannot make temp file with prefix=${prefix}"
    }
    printf "%s\n" "${fn}"
}

function cleanup_temp_files() {
    case "${KEEP_PROMPT_TEMP_FILE}" in
        ALL)
            log_warn "Leaving temporary files: ${TMPFILES_TO_DELETE}"
            true
            ;;
        ERROR|ERRORS)
            if [ "$status" -ne 0 ]; then
                log_warn "Leaving temporary files: ${TMPFILES_TO_DELETE}"
            elif [ -n "${TMPFILES_TO_DELETE}" ]; then
                for file in "${TMPFILES_TO_DELETE[@]}"; do
                    cleanup_file "$file"
                done
            fi
            ;;
        NONE)
            for file in "${TMPFILES_TO_DELETE[@]}"; do
                cleanup_file "$file"
            done
            ;;
        *)
            log_warn "Invalid value for KEEP_PROMPT_TEMP_FILE: ${KEEP_PROMPT_TEMP_FILE}.  No cleanup performed."
            ;;
    esac
}

function cleanup_file() {
    local file="$1"
    log_info "Cleaning up temporary file: ${file}"
    [[ -z "$file" ]] && return 0
    [[ "$file" == "/dev/null" ]] && return 0
    [[ -d "$file" ]] && { log_warn "cleanup_file: skipping dir $file"; return 1; }
    rm -f "$file" || log_error "Error Removing $file" $?
}

function string_trim() {
    local s="$1"
    # Remove leading whitespace
    s="${s#"${s%%[![:space:]]*}"}"
    # Remove trailing whitespace
    s="${s%"${s##*[![:space:]]}"}"
    printf '%s' "$s"
}
