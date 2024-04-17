#!/bin/bash

SCRIPT_DIR=$(dirname $(realpath "${BASH_SOURCE}"))

LINK=${1:-}			# LAST
ARGS=${@:2}			# BUTLAST

. ${SCRIPT_DIR}/../models/functions.sh

if [ -z "$LINK" ]; then
    echo "Usage: $(basename $0) <LINK> [llm.sh options]"
    exit 1
fi

if command -v lynx &> /dev/null; then
    LYNX="lynx --dump --nolist"
elif command -v links &> /dev/null; then
    LYNX="links -codepage utf-8 -force-html -width 72 -dump"
else
    echo "error: NOLINKS"
    exit 1
fi

SUMMARIZE_SYSTEM_MESSAGE='Summarize the following web page article and ignore website header at the start and look for the main article.'
export SYSTEM_MESSAGE="${SYSTEM_MESSAGE:-$(printf "%b" "${SUMMARIZE_SYSTEM_MESSAGE}")}"

${LYNX} "${LINK}" | ${SCRIPT_DIR}/llm.sh --long ${ARGS} "# Text of link ${LINK}" 
