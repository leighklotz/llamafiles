#!/bin/bash

SCRIPT_DIR=$(dirname $(realpath "${BASH_SOURCE}"))

export SYSTEM_MESSAGE=$(printf "%b" "Answer the following user question, or follow the user instruction, about Mac, OSX, Bash, Python, general programming, or even other subjects, if you know the answer:\n")

exec ${SCRIPT_DIR}/llm.sh ${*}
