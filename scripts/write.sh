#!/bin/bash

SCRIPT_DIR=$(dirname $(realpath "${BASH_SOURCE}"))

# This prompt is copy-pasted from iTerm2's proprietary OpenAI Help feature

#export LINUX_WRITE_SYSTEM_MESSAGE="${LINUX_WRITE_SYSTEM_MESSAGE:-Return commands suitable for copy/pasting into $(echo $SHELL) on $(uname). Do NOT include commentary NOR Markdown triple-backtick code blocks as your whole response will be copied into my terminal automatically.\nThe script should do this:\n}"
export LINUX_WRITE_SYSTEM_MESSAGE="${LINUX_WRITE_SYSTEM_MESSAGE:-Output one or more bare shell commands to be exceuted directly by $SHELL on $(uname). Use comment syntax for everything except executable shell commands.\nThe commands should do this:}"
LINUX_WRITE_SYSTEM_MESSAGE="$(printf "%b" "${LINUX_WRITE_SYSTEM_MESSAGE}")"

export SYSTEM_MESSAGE="${SYSTEM_MESSAGE:-${LINUX_WRITE_SYSTEM_MESSAGE}}"

${SCRIPT_DIR}/llm.sh "$@"
