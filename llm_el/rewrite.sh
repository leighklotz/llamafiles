#!/bin/bash

SCRIPT_DIR=$(dirname $(realpath "${BASH_SOURCE}"))
LLM_SH="${HOME}/wip/llamafiles/llm.sh"
DEBUG=""
PROMPT="${*}"

export SYSTEM_MESSAGE=$(printf "Re-write the following text according to user instructions:\n")
export MODEL_TYPE=mixtral

TEMPFILE=$(mktemp)
echo "${PROMPT}" > "${TEMPFILE}"
cat >> "${TEMPFILE}"

context_length=$(( $(wc -c < "${TEMPFILE}") / 4 ))
context_length=$((context_length < 2048 ? 2048 : context_length > 32768 ? 32768 : context_length))

cat "${TEMPFILE}" | ${LLM_SH} ${DEBUG} --context-length "${context_length}" --stdin "${PROMPT}" || (cat "${TEMPFILE}"; exit 1)
# rm "${TEMPFILE}"
