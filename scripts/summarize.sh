#!/bin/bash

SCRIPT_DIR=$(dirname $(realpath "${BASH_SOURCE}"))

LINK=${1:-}			# LAST
ARGS=${@:2}			# BUTLAST

post_process=""

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

binary_name="$(basename "${0}")"
case "${binary_name%.*}" in
    summarize)
	SYSTEM_MESSAGE="Summarize the following web page article and ignore website header at the start and look for the main article."
	post_process=summarize
	;;
    scuttle)
	SYSTEM_MESSAGE="Summarize the following web page article at the specified link address address and give link, title, description, and tags as JSON."
	post_process=scuttle
	;;
    *)
	echo "$binary_name: unrecognized binary name"
	exit 1
esac

export SYSTEM_MESSAGE=$(printf "%b" "${SYSTEM_MESSAGE}")

function post_process {
    case "$post_process" in
	summarize)
	;;
	scuttle)
	    # <https://scuttle.klotz.me/bookmarks/klotz?action=add&address=https://example.com&title=Example+Website+&description=This+is+an+example+website&tags=example,website,canonical+page>
	    jq --arg xspace "%20" --arg plus "+" --arg xcomma "%2[cC]" --arg comma "," -r '.tags |= join(",") | "https://scuttle.klotz.me/bookmarks/klotz?action=add&address=\(.link|@uri|gsub($xspace; $plus)|gsub($xcomma; $comma))&description=\(.description|@uri|gsub($xspace; $plus)|gsub($xcomma; $comma))&title=\(.title|@uri|gsub($xspace; $plus)|gsub($xcomma; $comma))&tags=\(.tags|@uri|gsub($xspace; $plus)|gsub($xcomma; $comma))"'
	;;
	*)
	    echo "* $0: unknown post_process" >> /dev/stderr
	    exit 1
    esac
}

${LYNX} "${LINK}" | ${SCRIPT_DIR}/llm.sh --length ${ARGS} "# Text of link <${LINK}>" | post_process

# todo: need to protect ${ARGS} better
