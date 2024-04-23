#!/bin/bash

SCRIPT_DIR=$(dirname $(realpath "${BASH_SOURCE}"))

# todo: character context for chat-instruct model preset
# "I am an AI bot designed to assist you with various tasks, including answering questions, providing information, and executing commands. I can help you with Linux, Bash, Python, general programming, and other subjects. Just ask and I'll do my best to assist you."

LINUX_HELP_SYSTEM_MESSAGE="$(printf "%b" "Answer the following user question, or follow the user instruction, about Linux, Bash, Python, general programming, or even other subjects, if you know the answer.\n")"
export SYSTEM_MESSAGE="${SYSTEM_MESSAGE:-${LINUX_HELP_SYSTEM_MESSAGE}}"

exec ${SCRIPT_DIR}/llm.sh "$@"
