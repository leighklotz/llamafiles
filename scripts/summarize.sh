#!/bin/bash

SCRIPT_DIR=$(dirname $(realpath "${BASH_SOURCE}"))

URL=${1:-}			# LAST
ARGS=${@:2}			# BUTLAST

if [ -z "$URL" ]; then
    echo "Usage: $(basename $0) <URL> [llm.sh options]"
    exit 1
fi

if command -v lynx &> /dev/null; then
    CMD="lynx --dump --nolist"
elif command -v links &> /dev/null; then
    CMD="links -codepage utf-8 -force-html -width 72 -dump"
else
    echo "error: NOLINKS"
    exit 1
fi

export SYSTEM_MESSAGE=$(printf "%b" "Summarize the following web page article and ignore website header at the start and look for the main article:\n#### Text of ${URL}\n###\n")

# somehow need to protect ${ARGS} better
${CMD} "${URL}" | ${SCRIPT_DIR}/llm.sh --length "${ARGS}"

