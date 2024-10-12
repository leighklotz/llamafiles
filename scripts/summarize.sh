#!/bin/bash

SCRIPT_DIR="$(dirname "$(realpath "${BASH_SOURCE}")")"
CAPTURE_COMMAND="cat"

if [ "$1" == "--capture-file" ]; then
    shift
    printf -v CAPTURE_COMMAND "tee %b" "$1"
    shift
fi

LINK=${1:-}			# LAST
ARGS=${@:2}			# BUTLAST

. ${SCRIPT_DIR}/../via/functions.sh

if [ -z "$LINK" ]; then
    echo "Usage: $(basename $0) [--capture-file fn] <LINK> [llm.sh options]"
    exit 1
fi

if [ "${LINK}" == "-" ]; then
    LYNX="cat"
elif command -v lynx &> /dev/null; then
    LYNX="lynx --dump --nolist"
elif command -v links &> /dev/null; then
    LYNX="links -codepage utf-8 -force-html -width 72 -dump"
else
    echo "error: NOLINKS"
    exit 1
fi

SUMMARIZE_SYSTEM_MESSAGE='Summarize the following web page article and ignore website header at the start and look for the main article.'
export SYSTEM_MESSAGE="${SYSTEM_MESSAGE:-$(printf "%b" "${SUMMARIZE_SYSTEM_MESSAGE}")}"

${LYNX} "${LINK}" | ${CAPTURE_COMMAND} | ${SCRIPT_DIR}/llm.sh --long ${ARGS} "# Text of link ${LINK}" 
