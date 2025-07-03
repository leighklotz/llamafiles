#!/bin/bash

# fetcher.sh - Fetches content from a URL using available tools

SCRIPT_DIR="$(dirname "$(realpath "${BASH_SOURCE}")")"
. "${SCRIPT_DIR}/../via/functions.sh"

USER_AGENT_TEMPLATE="ScuttleService/1.0 (+https://github.com/hallux-ai/summarizer-service; %s) %s"
REFERER="https://scuttle.klotz.me"
# abuse@hallux.ai
ABUSE_EMAIL_ADDRESS="${ABUSE_EMAIL_ADDRESS:-klotz@klotz.me}"
: "${FETCHER:=}"   # downlink, links, lynx

usage() {
    echo "Usage: $(basename "$0") <URL>"
    exit 1
}

if [ -z "$1" ]; then
    usage
fi

URL="$1"
DOWNLINK_COMMAND="${SCRIPT_DIR}/downlink.py"

if [ -f "${SCRIPT_DIR}/.venv/bin/activate" ]; then
    . "${SCRIPT_DIR}/.venv/bin/activate"
fi

if [ -n "${FETCHER}" ]; then
    true
elif [ -x "${DOWNLINK_COMMAND}" ]; then
    FETCHER="downlink"
elif command -v lynx &> /dev/null; then
    FETCHER="lynx"
    fetch_version="$(lynx -version 2>&1 | head -1)"
    if [[ $fetch_version =~ Lynx\ Version\ ([0-9a-zA-Z.]+) ]]; then
        fetch_version="Lynx/${BASH_REMATCH[1]}"
    fi
elif command -v links &> /dev/null; then
    FETCHER="links"
    fetch_version="$(links -version 2>&1 | head -1)"
    if [[ $fetch_version =~ Links\ ([0-9a-zA-Z.]+) ]]; then
        fetch_version="Links/${BASH_REMATCH[1]}"
    fi
else
    echo "error: NOLINKS"
    exit 1
fi

USER_AGENT=$(printf "$USER_AGENT_TEMPLATE" "$ABUSE_EMAIL_ADDRESS" "$fetch_version")

if [ -e "${fetch_version}" ]; then
    echo "Could not find the Lynx/Links version number."
    exit 1
fi

log_info "${FETCHER} fetching <${URL}>"
case "${FETCHER}" in
    downlink)
        "${DOWNLINK_COMMAND}" "${URL}" --user-agent "${USER_AGENT}"
        ;;
    lynx)
        lynx --dump --nolist -useragent="${USER_AGENT}" "${URL}"
        ;;
    links)
        links -codepage utf-8 -force-html -width 72 -dump -http.fake-user-agent "${USER_AGENT}" -http.fake-referer "${REFERER}" "${URL}"
        ;;
    *)
        log_and_exit "NOLINKS: FETCHER=$FETCHER"
        exit 1
        ;;
esac

