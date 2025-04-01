#!/bin/bash

SCRIPT_DIR="$(dirname "$(realpath "${BASH_SOURCE}")")"

REWRITE_LINUX_HELP_SYSTEM_MESSAGE="Rewrite this Production Engineer Assistant's request to be a brief LLM prompt to get a working program quickly, in either Python or Bash and Awk/sed/grep etc.\

The goal for PEAS is to give brief, safe, easily verified, working code to solve problems. Generalize to command flags and the like as appropriate, instead of hard-coding constants."

exec ${SCRIPT_DIR}/help.sh -i "$@"

