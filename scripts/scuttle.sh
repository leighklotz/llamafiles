#!/bin/bash

SCRIPT_DIR="$(dirname "$(realpath "${BASH_SOURCE}")")"
CAPTURE_COMMAND="cat"

. "${SCRIPT_DIR}/../via/functions.sh"

: "${ABUSE_EMAIL_ADDRESS:=klotz@klotz.me}"
# : "${ABUSE_EMAIL_ADDRESS:=abuse@hallux.ai}"
: "${SCUTTLE_USER_AGENT:=ScuttleService/1.0 (+https://github.com/hallux-ai/summarizer-service; ${ABUSE_EMAIL_ADDRESS}) ${fetch_version}}"
: "${SCUTTLE_REFERER:=https://scuttle.klotz.me}"

function usage() {
    echo "Usage: $(basename "$0") [--json | --yaml | --link] [--capture-file file] <LINK>|- [llm.sh options]\nCode: $1"
    exit 1
}

if command -v "yq" >/dev/null 2>&1; then
    JQYQ="yq"
    INTERMEDIATE_FORMAT="YAML"
elif command -v "jq" >/dev/null 2>&1; then
    JQYQ="jq"
    INTERMEDIATE_FORMAT="json"
else
    log_error "Error: cannot find jq or yq"
    exit 1
fi

OUTPUT_MODE='LINK'

while true; do
    case "$1" in
        "--help")
            usage "HELP"
            exit 1
            ;;
        "--link")
            OUTPUT_MODE='LINK'
            shift
            ;;
        "--yaml")
            JQYQ="yq"
            OUTPUT_MODE='YAML'
            INTERMEDIATE_FORMAT='YAML'
            shift
            ;;
        "--json")
            JQYQ="jq"
            OUTPUT_MODE='JSON'
            INTERMEDIATE_FORMAT='JSON'
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

log_info "JQYQ=$JQYQ"

if [ -z "$LINK" ]; then
    usage "NOLINK"
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

function fetch_text() {
    local url="$1"
    if [ "${fetcher}" == "lynx" ]; then
        # todo: support referer in lynx via -cfg file
        # todo: lynx complains about -useragent by design so redirect errors
        lynx --dump --nolist -useragent="${SCUTTLE_USER_AGENT}" "${url}" 2> /dev/null
    elif [ "${fetcher}" == "links" ]; then
        links -codepage utf-8 -force-html -width 72 -dump -http.fake-user-agent "${SCUTTLE_USER_AGENT}" -http.fake-referer "${SCUTTLE_REFERER}" "${url}"
    else
        log_and_exit 3 "error: NOLINKS: fetcher=$fetcher"
        exit 3
    fi
}

function extract_output() {
    case "$OUTPUT_MODE" in
        "JSON")
            cat
            ;;
        "YAML")
            cat
            ;;
        "LINK")
            to_link
            ;;
        *)
            usage "BAD OUTPUT_MODE=$OUTPUT_MODE"
            ;;
    esac
}

# # transforms JSON output from LLM into a properly formatted URL string for Scuttle bookmark adding.
# function scuttle_extract() {
#     if [ "${OUTPUT_MODE}" == 'YAML' ]; then
#         cat | remove_code_fence | replace_smart_quotes
#         return $?
#     elif [ "${OUTPUT_MODE}" == 'JSON' ]; then
#         INTERMEDIATE_FORMAT='JSON'
#         cat | remove_code_fence | replace_smart_quotes
#         return $?
#     elif [ "${OUTPUT_MODE}" == 'LINK' ]; then
#         cat | remove_code_fence | replace_smart_quotes | to_link
#         return $?
#     else
#         usage xxx
#     fi
# }

function to_link() {
    # <https://scuttle.klotz.me/bookmarks/klotz?action=add&address=https://example.com&title=Example+Website+&description=This+is+an+example+website&tags=example,website,canonical+page>
    cat | "${JQYQ}" --arg xspace "%20" --arg plus "+" --arg xcomma "%2[cC]" --arg comma "," -r '.keywords |= if(type == "array") then join(",") else . end | "https://scuttle.klotz.me/bookmarks/klotz?action=add&address=\(.link|@uri|gsub($xspace; $plus)|gsub($xcomma; $comma))&description=\(.description|@uri|gsub($xspace; $plus)|gsub($xcomma; $comma))&title=\(.title|@uri|gsub($xspace; $plus)|gsub($xcomma; $comma))&tags=\(.keywords|@uri|gsub($xspace; $plus)|gsub($xcomma; $comma))"'
}

function postprocess() {
    if [ -n "$VERBOSE" ]; then
        tee /dev/stderr | remove_code_fence | replace_smart_quotes | extract_output
    else
                          remove_code_fence | replace_smart_quotes | extract_output
    fi
    s=$?
    [ $? != 0 ] && log_error "postprocess"
    return $s
}

function remove_code_fence()  {
    sed 's/```.*$//g; /^[ \t]*$/d'
}

# remove smart quotes, as they cause parsing errors
function replace_smart_quotes() {
    cat | sed -e 's/[“”]/"/g'
}

if [ -z "${INHIBIT_GRAMMAR}" ] && [ "${OUTPUT_MODE}" != "YAML" ]; then
    GRAMMAR_FLAG="--grammar-file ${SCRIPT_DIR}/json3.gbnf"
else
    GRAMMAR_FLAG=""
fi

POST_PROMPT_ARG="Respond with only a short ${INTERMEDIATE_FORMAT} object with these 4 fields: "'`link`, `title`, `description`, and `keywords` array'
LINKS_PRE_PROMPT="Below is a web page article from the specified link address. Follow the instructions after the article."
SCUTTLE_POST_PROMPT="Read the above web page article from ${LINK} and ignore website header at the start and look for the main article."

( printf "# Text of link %s\n" "${LINK}"; fetch_text "${LINK}" | ${CAPTURE_COMMAND}; \
  printf "\n# Instructions\n%b\n%b\n" "${SCUTTLE_POST_PROMPT}" "${POST_PROMPT_ARG}" ) | \
    "${SCRIPT_DIR}/llm.sh" --long ${GRAMMAR_FLAG} ${ARGS} "${LINKS_PRE_PROMPT}" | \
    postprocess
