#!/bin/bash

SCRIPT_DIR="$(dirname "$(realpath "${BASH_SOURCE}")")"
CAPTURE_COMMAND="cat"

. "${SCRIPT_DIR}/../via/functions.sh"

function usage() {
    echo "Usage: $(basename "$0") [--capture-file file] <LINK>|- [llm.sh options]"
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
	"--capture-file")
	    shift
	    printf -v CAPTURE_COMMAND "tee %b" "$1"
	    shift
	    ;;
	*)
	    LINK="$1"
	    shift
	    ARGS="$*"
	    break
	    ;;
    esac
done

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
    log_and_exit 1 "error: NOLINKS"
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
	lynx --dump --nolist -useragent="${SCUTTLE_USER_AGENT}" -header="Referer: ${SCUTTLE_REFERER}" "${url}"
    elif [ "${fetcher}" == "links" ]; then
	links -codepage utf-8 -force-html -width 72 -dump -http.fake-user-agent "${SCUTTLE_USER_AGENT}" -http.fake-referer "${SCUTTLE_REFERER}" "${url}"
    else
	log_and_exit 3 "error: NOLINKS: fetcher=$fetcher"
	exit 3
    fi
}

# replace all '```json`' and '```' with empty. hope that's enough and we don't ahve to get stateful.
function preprocess_markdown {
    cat | sed -n '/{/,/}/p' | sed '1h;1!H;$!d;g;s/.*\({.*}\).*/\1/'
}

# fixme: this removes quotes inside JSON strings as well, which will cause errors if they exist.
function remove_smart_quotes {
    cat | sed -e 's/[“”]/"/g'
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

POST_PROMPT_ARG='Respond with only a short JSON object with these 4 fields: `link`, `title`, `description`, and `keywords` array'
LINKS_PRE_PROMPT="Below is a web page article from the specified link address. Follow the instructions after the article."
SCUTTLE_POST_PROMPT="Read the above web page article from ${LINK} and ignore website header at the start and look for the main article."

( printf "# Text of link %s\n" "${LINK}"; fetch_text "${LINK}" | ${CAPTURE_COMMAND}; printf "\n# Instructions\n%b\n%b\n" "${SCUTTLE_POST_PROMPT}" "${POST_PROMPT_ARG}") | \
    "${SCRIPT_DIR}/llm.sh" --long ${GRAMMAR_FLAG} ${ARGS} "${LINKS_PRE_PROMPT}"| \
    post_process
