#!/bin/bash

SCRIPT_DIR="$(dirname "$(realpath "${BASH_SOURCE}")")"

. "${SCRIPT_DIR}/../via/functions.sh"

function usage() {
    echo "Usage: $(basename "$0") [--json] <LINK> [llm.sh options]"
    exit 1
}

JSON_MODE=""

while true;
do
    case "$1" in
	"--help")
	    usage
	    exit 1
	    ;;
	"--json")
	    JSON_MODE=1
	    shift
	    ;;
	*)
	    LINK=$1
	    shift
	    ARGS="$*"
	    break
	    ;;
    esac
done

if [ -z "$LINK" ];
then
    usage
fi

if command -v lynx &> /dev/null;
then
    LYNX="lynx --dump --nolist"
elif command -v links &> /dev/null;
then
    LYNX="links -codepage utf-8 -force-html -width 72 -dump"
else
    echo "error: NOLINKS"
    exit 1
fi

SCUTTLE_SYSTEM_MESSAGE='Summarize the web page article at the specified link address. Respond with only a short JSON object with these 4 fields: `link`, `title`, `description`, and `keywords` array:'
export SYSTEM_MESSAGE="${SYSTEM_MESSAGE:-$(printf "%b" "${SCUTTLE_SYSTEM_MESSAGE}")}"

# replace all '```json`' and '```' with empty. hope that's enough and we don't ahve to get stateful.
function preprocess_markdown {
    cat | sed -n '/{/,/}/p' | sed '1h;1!H;$!d;g;s/.*\({.*}\).*/\1/'
}

# fixme: this removes quotes inside JSON strings as well, which will cause errors if they exist.
function remove_smart_quotes {
    cat | sed -e 's/[“”]/"//g'
}

# transforms JSON output from LLM into a properly formatted URL string for Scuttlebookmark adding.
# todo: try to find the json in the input, for example inside backquotes
#       or force json schema
function scuttle_extract_json {
    if [ -n "${JSON_MODE}" ]; then
	cat | preprocess_markdown | remove_smart_quotes
	return $?
    else
	cat | preprocess_markdown | remove_smart_quotes | jq --arg xspace "%20" --arg plus "+" --arg xcomma "%2[cC]" --arg comma "," -r '.keywords |= if(type == "array") then join(",") else . end | "https://scuttle.klotz.me/bookmarks/klotz?action=add&address=\(.link|@uri|gsub($xspace; $plus)|gsub($xcomma; $comma))&description=\(.description|@uri|gsub($xspace; $plus)|gsub($xcomma; $comma))&title=\(.title|@uri|gsub($xspace; $plus)|gsub($xcomma; $comma))&tags=\(.keywords|@uri|gsub($xspace; $plus)|gsub($xcomma; $comma))"'
	return $?
    fi
}

function post_process {
    # <https://scuttle.klotz.me/bookmarks/klotz?action=add&address=https://example.com&title=Example+Website+&description=This+is+an+example+website&tags=example,website,canonical+page>
    if [ -n "$VERBOSE" ];
    then
	log_debug "post_process"
	tee /dev/stderr | scuttle_extract_json
    else
	scuttle_extract_json
    fi
    s=$?
    [ $? != 0 ] && log_error "post_process"
    return $s
}

if [ -z "${INHIBIT_GRAMMAR}" ];
   then
       GRAMMAR_FLAG="--grammar-file ${SCRIPT_DIR}/json3.gbnf"
else
    GRAMMAR_FLAG=""
fi

(${LYNX} "${LINK}"; printf "\n# Instruction\n%s\n" "${SCUTTLE_SYSTEM_MESSAGE}")| "${SCRIPT_DIR}/llm.sh" --long ${GRAMMAR_FLAG} ${ARGS} "# Text of link ${LINK}" | post_process
