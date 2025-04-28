#!/bin/bash

SCRIPT_DIR="$(dirname "$(realpath "${BASH_SOURCE}")")"
CAPTURE_COMMAND="cat"

. "${SCRIPT_DIR}/../via/functions.sh"

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
# todo: actually everything up to a '--' should go into POST_PROMPT and everything after '--' should go into the ARGS.
POST_PROMPT_ARG="${@:2}"        # BUTLAST
: "${POST_PROMPT_ARG:=Summarize:}"
ARGS=''

if [ -z "$LINK" ]; then
    usage
fi

if [ "${LINK}" == "-" ]; then
    fetcher="cat"
elif command -v lynx &> /dev/null; then
    fetcher="lynx"
    fetch_version="$(lynx -version 2>&1 | head -1)"
    if [[ $fetch_version =~ Lynx\ Version\ ([0-9a-zA-Z.]+) ]]; then
        fetch_version="Lynx/${BASH_REMATCH[1]}"
    fi
elif command -v links &> /dev/null; then
    fetcher="links"
    fetch_version="$(links -version 2>&1 | head -1)"
    if [[ $fetch_version =~ Links\ ([0-9a-zA-Z.]+) ]]; then
        fetch_version="Links/${BASH_REMATCH[1]}"
    fi
else
    echo "error: NOLINKS"
    exit 1
fi

if [ -e "${fetch_version}" ]; then
    log_and_exit 2 "Could not find the Lynx/Links version number."
fi

# until we get it working
: "${ABUSE_EMAIL_ADDRESS:=klotz@klotz.me}"
# : "${ABUSE_EMAIL_ADDRESS:=abuse@hallux.ai}"
: "${SCUTTLE_USER_AGENT:=ScuttleService/1.0 (+https://github.com/hallux-ai/summarizer-service; ${ABUSE_EMAIL_ADDRESS}) ${fetch_version}}"
: "${SCUTTLE_REFERER:=https://scuttle.klotz.me}"

function fetch_text() {
    local url="$1"
    if [ "${fetcher}" == "lynx" ]; then
        # todo: support referer in lynx via -cfg file
        lynx --dump --nolist -useragent="${SCUTTLE_USER_AGENT}" "${url}"
    elif [ "${fetcher}" == "links" ]; then
        links -codepage utf-8 -force-html -width 72 -dump -http.fake-user-agent "${SCUTTLE_USER_AGENT}" -http.fake-referer "${SCUTTLE_REFERER}" "${url}"
    else
        echo "error: NOLINKS: fetcher=$fetcher"
        exit 1
    fi
}

LINKS_PRE_PROMPT="Below is a web page article from <${LINK}>. If it does not have content, very briefly report the failure. Otherwise, follow the instructions after the article."
SUMMARIZE_POST_PROMPT="Read the above web page article from <${LINK}>. If it does not have content, very briefly report the failure. Otherwise, follow these instructions:\n"

( printf "# Text of link %s\n" "${LINK}"; 
  fetch_text "${LINK}" | ${CAPTURE_COMMAND};
  printf "\n# Instructions\n%b\n%b\n" "${SUMMARIZE_POST_PROMPT}" "${POST_PROMPT_ARG}") \
| "${SCRIPT_DIR}/llm.sh" ${ARGS} "${LINKS_PRE_PROMPT}"
