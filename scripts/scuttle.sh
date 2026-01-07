#!/bin/bash

SCRIPT_DIR="$(dirname "$(realpath "${BASH_SOURCE}")")"
CAPTURE_COMMAND="cat"
FETCHER_COMMAND="${SCRIPT_DIR}/fetcher.sh"

. "${SCRIPT_DIR}/../via/functions.sh"

if command -v "yq" >/dev/null 2>&1; then
    JQYQ="yq"
    INTERMEDIATE_FORMAT="YAML"
elif command -v "jq" >/dev/null 2>&1; then
    JQYQ="jq"
    INTERMEDIATE_FORMAT="json"
else
    log_error 'Error: cannot find jq or yq: do `pip install yq` and do not use yq snap'
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

function extract_output() {
    case "$OUTPUT_MODE" in
        "JSON")
            # awk out just the JSON '{}' objects.
            awk '/{/{f=1} f; /}/{f=0}'
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

function to_link() {
    # <https://scuttle.klotz.me/bookmarks/klotz?action=add&address=https://example.com&title=Example+Website+&description=This+is+an+example+website&tags=example,website,canonical+page>
    cat | "${JQYQ}" --arg xspace "%20" --arg plus "+" --arg xcomma "%2[cC]" --arg comma "," -r '.keywords |= if(type == "array") then join(",") else . end | "https://scuttle.klotz.me/bookmarks/klotz?action=add&address=\(.link|@uri|gsub($xspace; $plus)|gsub($xcomma; $comma))&description=\(.description|@uri|gsub($xspace; $plus)|gsub($xcomma; $comma))&title=\(.title|@uri|gsub($xspace; $plus)|gsub($xcomma; $comma))&tags=\(.keywords|@uri|gsub($xspace; $plus)|gsub($xcomma; $comma))"'
}

function capture() {
    if [[ $? -ne 0 ]]; then
        log_and_exit "$?" "$(cat)"
    fi
  ${CAPTURE_COMMAND}
}

function postprocess() {
    if [ -n "$VERBOSE" ]; then
        tee /dev/stderr | remove_code_fence | replace_smart_quotes | extract_output
    else
                          remove_code_fence | replace_smart_quotes | extract_output
    fi
    s=$?
    [ $s != 0 ] && log_error "postprocess"
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

: "${LINKS_FETCH_PROMPT:=Below is the text of a web page article from the specified link address. If retrieval failed, report on the failure. Otherwise:}"
: "${SCUTTLE_ARTICLE_PROMPT:=Read the web page article from ${LINK} and ignore website header at the start and look for the main article.}"
: "${RESPONSE_FORMAT_PROMPT:=Respond with only a short ${INTERMEDIATE_FORMAT} object with these 4 fields: link, title, description, keywords (array)}"

printf -v INITIAL_PROMPT "\n# Instructions\n- %b\n- %b\n- %b\n" "${LINKS_FETCH_PROMPT}" "${SCUTTLE_ARTICLE_PROMPT}" "${RESPONSE_FORMAT_PROMPT}"

printf "# Text of link %s\n%s" "${LINK}" "$("${FETCHER_COMMAND}" "${LINK}")" | \
        capture | \
        "${SCRIPT_DIR}/llm.sh" ${GRAMMAR_FLAG} ${ARGS} "${INITIAL_PROMPT}" | \
        postprocess
