#!/bin/bash

SCRIPT_DIR=$(dirname $(realpath "${BASH_SOURCE}"))

LINK=${1:-}			# LAST
ARGS=${@:2}			# BUTLAST

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
	;;
    scuttle)
	SYSTEM_MESSAGE='Give title, brief summary as bullet points, and tags of the retrieved web page and convert your output to a URL in the following format, using `+` for space: `<https://scuttle.klotz.me/bookmarks/klotz?action=add&address=LINK&title=TITLE+WORDS+&description=SUMMARY+TEXT&tags=tag1,tag+two,tag3>`'
	#SYSTEM_MESSAGE='Output a URL in the following format, using `+` for space: `<https://scuttle.klotz.me/bookmarks/klotz?action=add&address=LINK&title=TITLE+WORDS+&description=SUMMARY+TEXT&tags=tag1,tag+two,tag3>`'
	;;
    *)
	echo "$binary_name: unrecognized binary name"
	exit 1
esac

export SYSTEM_MESSAGE=$(printf "%b" "${SYSTEM_MESSAGE}")

${LYNX} "${LINK}" | ${SCRIPT_DIR}/llm.sh --length ${ARGS} "# Text of link <${LINK}>"

# todo: need to protect ${ARGS} better
