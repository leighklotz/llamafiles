#!/bin/bash

SCRIPT_DIR=$(dirname $(realpath "${BASH_SOURCE}"))

export SYSTEM_MESSAGE=$(printf "%b" "Answer the following user question about Linux, bash, python, or general programming:\n")

exec ${SCRIPT_DIR}/llm.sh ${*}
