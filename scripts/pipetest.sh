#!/usr/bin/env bash

# pipetest alias – forward piped data after a Y/N confirmation
# -----------------------------------------------------------------------
#   Read all data from standard input (the "pipe").
#   Ask the user "Y or N? " (case‑insensitive, newline required).
#   If the answer starts with "y" (or "yes") the data is forwarded
#   to stdout.  Otherwise nothing is written.
# -----------------------------------------------------------------------

# This file is intended to be *sourced*.
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
  echo "ERROR: This script must be sourced, not executed."
  exit 1
fi

function pipetest() {
    local user_query="$*"

    # 1. Capture stdin into a temporary file – this allows very large input.
    local tmpfile
    if ! tmpfile=$(mktemp --tmpdir="$(mktemp -d)" pipetest.XXXXXX 2>/dev/null) ; then
        printf >&2 "pipetest: could not create temporary file\n"
        exit 1
    fi
    trap 'rm -f "$tmpfile"' EXIT

    cat >"$tmpfile"


    # 3. Prompt from stderr (visible in the terminal) and read a full line.
    local reply
    (printf "🤖 "; head -10 "$tmpfile"; printf "🤖 %s: Y or N? " "$user_query") >&2 
    read -r -t 0 -s reply < /dev/tty     # non‑blocking: only keep the first char
    # If the first char is not a 'y'/'Y', read the rest of the line to discard it.
    if [[ ! "${reply}" =~ ^[yY]$ ]] ; then
        # Drain the remainder of the line (including the newline)
        read -r reply < /dev/tty
    fi
    printf "\n" >&2

    # 4. If the first character is 'y' or 'Y', output the captured data.
    case "${reply,,}" in
        y*) cat "$tmpfile" ;;
        *) printf "🚫 discarded\n" >&2 ;;
    esac
}
