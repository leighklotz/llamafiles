#!/bin/bash

# fetcher.sh - Fetches content from a URL using available tools

SCRIPT_DIR="$(dirname "$(realpath "${BASH_SOURCE}")")"
VENV_ACTIVATE="${SCRIPT_DIR}/.venv/bin/activate"
. "${SCRIPT_DIR}/../via/functions.sh"

DEFAULT_USER_AGENT='Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/111.0.0.0 Safari/537.36'
USER_AGENT="${USER_AGENT:-${DEFAULT_USER_AGENT}}"
REFERER="https://scuttle.klotz.me"
: "${FETCHER:=}"   # downlink, links, lynx

usage() {
    echo "Usage: $(basename "$0") <URL>"
    echo "Obeys \$USER_AGENT"
    exit 1
}

URL="$1"

if [ -z "$URL" ]; then
    usage
fi

if command -v downlink &> /dev/null;
then
    DOWNLINK_COMMAND="$(command -v downlink)"
else
    DOWNLINK_COMMAND=""
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
    if [ -e "${fetch_version}" ]; then
        echo "Could not find the Lynx/Links version number."
        exit 2
    fi
elif command -v links &> /dev/null; then
    FETCHER="links"
    fetch_version="$(links -version 2>&1 | head -1)"
    if [[ $fetch_version =~ Links\ ([0-9a-zA-Z.]+) ]]; then
        fetch_version="Links/${BASH_REMATCH[1]}"
    fi
    if [ -e "${fetch_version}" ]; then
        echo "Could not find the Lynx/Links version number."
        exit 3
    fi

else
    echo "error: NOLINKS"
    exit 4
fi


log_info "${FETCHER} fetching <${URL}>"
case "${FETCHER}" in
    downlink)
        if [ -z "${USER_AGENT}" ]; then
            "${DOWNLINK_COMMAND}" "${URL}"
        else
            "${DOWNLINK_COMMAND}" "${URL}" --user-agent "${USER_AGENT}"
        fi
        ;;
    lynx)
        lynx --dump --nolist -useragent="${USER_AGENT}" "${URL}"
        ;;
    links)
        links -codepage utf-8 -force-html -width 72 -dump -http.fake-user-agent "${USER_AGENT}" -http.fake-referer "${REFERER}" "${URL}"
        ;;
    *)
        log_and_exit 5 "NOLINKS: FETCHER=$FETCHER"
        ;;
esac
