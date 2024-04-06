#!/bin/bash

SCRIPT_DIR=$(dirname $(realpath "${BASH_SOURCE}"))

HELP_SYSTEM_MESSAGE="$(printf "%b" "Answer the following user question, or follow the user instruction, about Linux, Bash, Python, general programming, or even other subjects, if you know the answer.\n")"
export SYSTEM_MESSAGE="${SYSTEM_MESSAGE:-${HELP_SYSTEM_MESSAGE}}"


exec ${SCRIPT_DIR}/llm.sh "$@"

