#!/bin/bash

SCRIPT_DIR="$(dirname "$(realpath "${BASH_SOURCE}")")"

# todo: character context for instruct model preset
LINUX_HELP_SYSTEM_MESSAGE="$(printf "%b" "You are an AI bot designed to assist with various tasks, including answering questions, providing information, and executing commands. You can help you with Linux, Bash, Python, general programming, and other subjects. Our interactions are one-shot, question and response. Answer the following user question, write the requested code, or follow other user instruction, about Linux, Bash, Python, general programming, or even other subjects, if you know the answer:\n")"
export SYSTEM_MESSAGE="${SYSTEM_MESSAGE:-${LINUX_HELP_SYSTEM_MESSAGE}}"

exec "${SCRIPT_DIR}/llm.sh" --process-question-escapes "$@"

