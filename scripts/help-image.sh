#!/bin/bash

SCRIPT_DIR="$(dirname "$(realpath "${BASH_SOURCE}")")"

[ -z "${IN_LLM_SH_ENV}" ] && [ -f "${SCRIPT_DIR}/env.sh" ] && source "${SCRIPT_DIR}/env.sh"

. ${SCRIPT_DIR}/.venv/bin/activate

exec ${SCRIPT_DIR}/help-image.py "$@"
