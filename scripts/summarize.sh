#!/bin/bash

SCRIPT_DIR="$(dirname "$(realpath "${BASH_SOURCE}")")"
CAPTURE_COMMAND="cat"
FETCHER_COMMAND="${SCRIPT_DIR}/fetcher.sh"

. "${SCRIPT_DIR}/../via/functions.sh"

if [ -f "${SCRIPT_DIR}/.venv/bin/activate" ]; then
    . "${SCRIPT_DIR}/.venv/bin/activate"
fi

function usage() {
    echo "Usage: $(basename "$0") [--capture-file file] <LINK>|- [llm.sh options]"
    exit 1
}

if [ "$1" == "--capture-file" ]; then
    shift
    printf -v CAPTURE_COMMAND "tee %b" "$1"
    shift
fi

LINK=${1:-}                     # LAST
# future todo: actually everything up to a '--' should go into POST_PROMPT and everything after '--' should go into the ARGS.
POST_PROMPT_ARG="${@:2}"        # BUTLAST
: "${POST_PROMPT_ARG:=Summarize:}"
ARGS=''

if [ -z "$LINK" ]; then
    usage
fi

LINKS_PRE_PROMPT="Below is a web page article from <${LINK}>. If it does not have content, very briefly report the failure. Otherwise, follow the instructions after the article."
SUMMARIZE_POST_PROMPT="Read the above web page article from <${LINK}>. If it does not have content, very briefly report the failure. Otherwise, follow these instructions:\n"

( printf "# Text of link %s\n" "${LINK}"; 
  "${FETCHER_COMMAND}" "${LINK}" | ${CAPTURE_COMMAND};
  printf "\n# Instructions\n%b\n%b\n" "${SUMMARIZE_POST_PROMPT}" "${POST_PROMPT_ARG}") \
| "${SCRIPT_DIR}/llm.sh" ${ARGS} "${LINKS_PRE_PROMPT}"
