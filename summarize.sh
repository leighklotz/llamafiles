#!/bin/bash

SCRIPT_DIR=$(dirname $(realpath "${BASH_SOURCE}"))

URL="${!#}"			# LAST
ARGS="${@:1:${#}-1}"		# BUTLAST

if LYNX=$(command -v lynx); then
   CMD="${LYNX} --dump"
elif LINKS=$(command -v links); then
   echo b
   CMD="${LINKS} -codepage utf-8 -force-html -width 72 -dump"
else
   echo "error: NOLINKS"
   exit 1
fi

export SYSTEM_MESSAGE=$(printf "%b" "Summarize the following web page article and ignore website header at the start and look for the main article:\n#### Text of ${URL}\n###\n")

${CMD} "${URL}" | ${SCRIPT_DIR}/llm.sh --stdin ${ARGS}
