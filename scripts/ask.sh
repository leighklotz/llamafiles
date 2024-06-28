#!/bin/bash

SCRIPT_DIR="$(dirname "$(realpath "${BASH_SOURCE}")")"

ASK_SYSTEM_MESSAGE="$(printf "%b" "Follow the user instruction:")"
export SYSTEM_MESSAGE="${SYSTEM_MESSAGE:-${ASK_SYSTEM_MESSAGE}}"

MODEL_TYPE=$1
shift
exec "${SCRIPT_DIR}/help.sh" -m "${MODEL_TYPE}" "$@"
