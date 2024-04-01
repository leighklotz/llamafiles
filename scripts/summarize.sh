#!/bin/bash

SCRIPT_DIR=$(dirname $(realpath "${BASH_SOURCE}"))

URL=${1:-}			# LAST
ARGS=${@:2}			# BUTLAST

if [ -z "$URL" ]; then
    echo "Usage: $(basename $0) <URL> [llm.sh options]"
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

case $(basename "${0%.*}") in
    summarize.sh|summarize)
	SYSTEM_MESSAGE="Summarize the following web page article and ignore website header at the start and look for the main article."
	;;
    scuttle.sh|scuttle)
	SYSTEM_MESSAGE='Give title, brief summary as bullet points, and tags of the retrieved web page and convert your output to a URL in the following format using `+` for space: `<https://scuttle.klotz.me/bookmarks/klotz?action=add&address=URL&title=TITLE+WORDS+&description=SUMMARY+TEXT&tags=tag1,tag+two,tag3>`'
	;;
    *)
	echo "$0: unrecognized binary name"
	exit 1
esac

export SYSTEM_MESSAGE=$(printf "%b" "${SYSTEM_MESSAGE}")

${LYNX} "${URL}" | ${SCRIPT_DIR}/llm.sh --length "${ARGS}" "# Text of <${URL}>"

# todo: need to protect ${ARGS} better
