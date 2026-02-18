#!/bin/bash

SCRIPT_DIR="$(dirname "$(realpath "${BASH_SOURCE}")")"
CAPTURE_COMMAND="cat"
FETCHER_COMMAND="${SCRIPT_DIR}/fetcher.sh"

# Requires snap/golang yq for yaml->json, and regular jq to extract
. "${SCRIPT_DIR}/../via/functions.sh"

if [ -f "${SCRIPT_DIR}/.venv/bin/activate" ]; then
    . "${SCRIPT_DIR}/.venv/bin/activate"
fi

OUTPUT_MODE='LINK'

# expecting something like /snap/bin/yq
# yq (https://github.com/mikefarah/yq) version v4.49.2
# log_info "yq=$(which yq)"
# log_info "jq=$(which jq)"

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
            OUTPUT_MODE='YAML'
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
    usage "NOLINK"
fi

function extract_output() {
    case "$OUTPUT_MODE" in
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
    jq_filter='
 def formenc:
    @uri
    | gsub("%20"; "+")
    | gsub("%2C"; ",");

  def csv_tags:
    if .keywords == null then ""
    elif (.keywords | type) == "array" then (.keywords | join(","))
    else .keywords
    end;

  "https://scuttle.klotz.me/bookmarks/klotz?action=add"
  + "&address="     + (.link        | formenc)
  + "&description=" + (.description | formenc)
  + "&title="       + (.title       | formenc)
  + "&tags="        + (csv_tags     | formenc)
'
  yaml=$(cat)
  log_verbose "yaml=$yaml"
  printf '%s\n' "$yaml" \
      | yq -r '.' -o=json \
      | jq -r "$jq_filter"
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

# Prompt is used twice, once before the text of link and once after.
SCUTTLE_PROMPT="# Instructions\nRead the web page article from ${LINK} and ignore website header at the start and look for the main article. If there are retrieval failures, just report on the failures. Otherwise, respond with only a properly-quoted YAML stanza with these 4 fields: "'`link`, `title`, `description`, and `keywords` array.'

( printf "# Text of link %s\n\n---\n\n%s\n" "${LINK}" "${SCUTTLE_PROMPT}";
  "${FETCHER_COMMAND}" "${LINK}" | ${CAPTURE_COMMAND};
  printf "%b\n" "${SCUTTLE_PROMPT}" ) | 
    "${SCRIPT_DIR}/llm.sh" ${ARGS} -- "${SCUTTLE_PROMPT}" | 
    postprocess
