#!/bin/bash

SCRIPT_DIR=$(dirname $(realpath "${BASH_SOURCE}"))

# This prompt is copy-pasted from iTerm2's proprietary OpenAI Help feature

#export WRITE_SYSTEM_MESSAGE="${WRITE_SYSTEM_MESSAGE:-Return commands suitable for copy/pasting into $(echo $SHELL) on $(uname). Do NOT include commentary NOR Markdown triple-backtick code blocks as your whole response will be copied into my terminal automatically.\nThe script should do this:\n}"

if [ -f /etc/os-release ];
   then
       pretty_name="$(grep PRETTY_NAME /etc/os-release | sed 's/PRETTY_NAME="\(.*\)".*/\1/')"
elif command -v sw_vers;
then
       pretty_name="$(sw_vers -productName; sw_vers -productVersion)"
else
    echo "$0: unknown system type"
    exit 1
fi

export WRITE_SYSTEM_MESSAGE="${WRITE_SYSTEM_MESSAGE:-Output a code block of one or more lines of ${SHELL} commands to be executed directly on $(uname) ${pretty_name}. Start all non-exceutable lines with comment syntax.\nThe commands should do this:}"
WRITE_SYSTEM_MESSAGE="$(printf "%b" "${WRITE_SYSTEM_MESSAGE}")"

export SYSTEM_MESSAGE="${SYSTEM_MESSAGE:-${WRITE_SYSTEM_MESSAGE}}"

${SCRIPT_DIR}/llm.sh "$@"
