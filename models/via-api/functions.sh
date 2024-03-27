#!/bin/bash -x

VIA_API_ENDPOINT='http://localhost:5000/v1/chat/completions'

# Check if the script is being sourced or directly executed
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    echo "This script '${BASH_SOURCE[0]}' is intended to be sourced, not executed directly."
    exit 1
fi

LLM_LIB_DIR=$(realpath "${SCRIPT_DIR}/../lib")
LLM_MODELS_DIR=$(realpath "${SCRIPT_DIR}/../models")
MODEL_FILE="mixtral/mixtral-8x7b-instruct-v0.1.Q5_K_M.llamafile"
PIDFILE="/tmp/via-api.pid"
EXTRA_ARGS="${EXTRA_ARGS:-}"

function via_api_prompt {
    if [ "${INPUT}" == "" ]; then
	printf -v PROMPT "%s" "${QUESTION%$'\n'}"
    else
	printf -v PROMPT "%s\n%s" "${QUESTION%$'\n'}" "${INPUT%$'\n'}"
    fi
}

function via_api_priority {
    true
}


# todo: make this be run only on mixtral, use as test case to separate api impl from model quirks
# fix mistral "\_" and "\*"
# https://www.reddit.com/r/LocalLLaMA/comments/1agrddy/has_anyone_encountered_mistrals_tendency_to_use/
function via_api_mistral_output_fixup {
    sed -e 's/\\_/_/g' | sed -e 's/\\\*/*/g'
}

# via_api_perform_inference "instruct" "You are a helpful math bot. Answer the user's questions." "${question}"
# via_api_perform_inference "$MODE" "$SYSTEM_PROMPT" "$QUESTION" "$GRAMMAR_FILE"
# todo: so many files and strings back and forth
function via_api_perform_inference() {
    local mode="$1" system_message="$2" question="$3" grammar_file="$4"
    local temperature="$5" repetition_penalty="$6" penalize_nl="$7"
    
    if [ "$grammar_file" == "" ];
    then
	grammar_file="/dev/null"
    fi
    #set -x
    question_file=$(mktemp); printf "%s" "${question}" >> "${question_file}"
    system_message_file=$(mktemp); printf "%s" "${system_message}" >> "${system_message_file}"
    data=$(jq --raw-input --raw-output  --compact-output -n \
	      --arg mode "${mode}" \
	      --arg temperature "${temperature}" \
	      --arg repetition_penalty "${repetition_penalty}" \
	      --arg penalize_nl "${penalize_nl}" \
	      --rawfile system_message "${system_message_file}" \
	      --rawfile question "${question_file}" \
	      --rawfile grammar_string "${grammar_file}" \
'{
    messages: [
      {
	role: "system",
	content: $system_message
      },
      {
	role: "user",
	content: $question
      }
    ],
    mode: $mode,
    temperature: $temperature,
    repetition_penalty: $repetition_penalty,
    penalize_nl: $penalize_nl,
}' \
	| jq 'del(.[] | select(. == ""))' \
	) || (s=$?; echo "* $0 FAIL: $s"; exit $s)
    #set -x
    if [ "${VERBOSE}" ]; then
	printf "%s\n" "${data}"
    fi
    result=$(printf "%s" "${data}" | curl -s "${VIA_API_ENDPOINT}" -H 'Content-Type: application/json' -d @-)
    output="$(printf "%s" "${result}" | jq --raw-output '.choices[].message.content')"
    rm -f "${question_file}" || echo "* WARN: unable to remove ${question_file}" >> /dev/stderr
    rm -f "${system_message_file}" || echo "* WARN: unable to remove ${system_message_file}" >> /dev/stderr
    printf "%s\n" "${output}" | via_api_mistral_output_fixup
}

function via_api_model {
    via_api_prompt
}
