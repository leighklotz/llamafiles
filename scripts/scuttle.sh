#!/bin/bash

SCRIPT_DIR=$(dirname $(realpath "${BASH_SOURCE}"))

LINK=${1:-}			# LAST
ARGS=${@:2}			# BUTLAST

. ${SCRIPT_DIR}/../models/functions.sh

if [ -z "$LINK" ]; then
    echo "Usage: $(basename $0) <LINK> [llm.sh options]"
    exit 1
fi

if command -v lynx &> /dev/null; then
    LYNX="lynx --dump --nolist"
elif command -v links &> /dev/null; then
    LYNX="links -codepage utf-8 -force-html -width 72 -dump"
else
    echo "error: NOLINKS"
    exit 1
fi

SCUTTLE_SYSTEM_MESSAGE='Summarize the following web page article at the specified link address address and give link, title, description, and keywords as a JSON object with keys `link`, `title`, `description`, and `keywords`.`'
export SYSTEM_MESSAGE="${SYSTEM_MESSAGE:-$(printf "%b" "${SCUTTLE_SYSTEM_MESSAGE}")}"

# transforms JSON output from LLM into a properly formatted URL string for Scuttlebookmark adding.
# todo: try to find the json in the input, for example inside backquotes
#       or force json schema
function scuttle_extract_json {
    jq --arg xspace "%20" --arg plus "+" --arg xcomma "%2[cC]" --arg comma "," -r '.keywords |= if(type == "array") then join(",") else . end | "https://scuttle.klotz.me/bookmarks/klotz?action=add&address=\(.link|@uri|gsub($xspace; $plus)|gsub($xcomma; $comma))&description=\(.description|@uri|gsub($xspace; $plus)|gsub($xcomma; $comma))&title=\(.title|@uri|gsub($xspace; $plus)|gsub($xcomma; $comma))&tags=\(.keywords|@uri|gsub($xspace; $plus)|gsub($xcomma; $comma))"'
    return $?;
}

function post_process {
    # <https://scuttle.klotz.me/bookmarks/klotz?action=add&address=https://example.com&title=Example+Website+&description=This+is+an+example+website&tags=example,website,canonical+page>
    if [ -n "$VERBOSE" ]; then
	log_debug "post_process"
	tee /dev/stderr | scuttle_extract_json
	s=$?
    else
	scuttle_extract_json
	s=$?
    fi
    [ $s != 0 ] && log_and_exit $s "$0" $s "post_process"
    return $s
}

${LYNX} "${LINK}" | ${SCRIPT_DIR}/llm.sh --long ${ARGS} "# Text of link ${LINK}" | post_process
