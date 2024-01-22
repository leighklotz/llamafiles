#!/bin/bash

SCRIPT_DIR=$(dirname $(realpath "${BASH_SOURCE}"))

export SYSTEM_MESSAGE=$(printf "%b" "Answer the following user question about Linux, Bash, Python, general programming, or even other subjects:\n")

exec ${SCRIPT_DIR}/llm.sh ${*}
