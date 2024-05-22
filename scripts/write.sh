#!/bin/bash

SCRIPT_DIR=$(dirname $(realpath "${BASH_SOURCE}"))

# This prompt is copy-pasted from iTerm2's proprietary OpenAI Help feature

#export WRITE_SYSTEM_MESSAGE="${WRITE_SYSTEM_MESSAGE:-Return commands suitable for copy/pasting into $(echo $SHELL) on $(uname). Do NOT include commentary NOR Markdown triple-backtick code blocks as your whole response will be copied into my terminal automatically.\nThe script should do this:\n}"
pretty_name="$(grep PRETTY_NAME /etc/os-release | sed 's/PRETTY_NAME="\(.*\)".*/\1/')"
export WRITE_SYSTEM_MESSAGE="${WRITE_SYSTEM_MESSAGE:-Output a code block of one or more lines of shell commands to be exceuted directly by ${SHELL} on $(uname) ${PRETTY_NAME}. Use comment syntax for all text that is not executable shell commands.\nThe commands should do this:}"
WRITE_SYSTEM_MESSAGE="$(printf "%b" "${WRITE_SYSTEM_MESSAGE}")"

export SYSTEM_MESSAGE="${SYSTEM_MESSAGE:-${WRITE_SYSTEM_MESSAGE}}"

${SCRIPT_DIR}/llm.sh "$@"
