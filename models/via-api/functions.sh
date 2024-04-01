#!/bin/bash -x

VIA_API_CHAT_COMPLETIONS_ENDPOINT='http://localhost:5000/v1/chat/completions'
VIA_API_MODEL_INFO_ENDPOINT='http://localhost:5000/v1/internal/model/info'
VIA_API_MODEL_LIST_ENDPOINT='http://localhost:5000/v1/internal/model/list'

# Check if the script is being sourced or directly executed
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    echo "This script '${BASH_SOURCE[0]}' is intended to be sourced, not executed directly."
    exit 1
fi

LLM_LIB_DIR=$(realpath "${SCRIPT_DIR}/../lib")
LLM_MODELS_DIR=$(realpath "${SCRIPT_DIR}/../models")
MODEL_FILE="mixtral/mixtral-8x7b-instruct-v0.1.Q5_K_M.llamafile"
PIDFILE="/tmp/via-api.pid"
SEED="${SEED:--1}"

# fixme: some models support the system role API and some do not.
# todo: query ooba API to find the model behind the api,
# and add put a new function to each model/*/*functions.sh
# to determinue USE_SYSTEM_ROLE properly for each model type.
# workaround: for now use `export USE_SYSTEM_ROLE=1` if you need it.
USE_SYSTEM_ROLE="${USE_SYSTEM_ROLE:-}"
# mixtral-7b-instruct-v0.1: no
# dolphin-2.6-mistral-7b-dpo: yes
# dolphin-2.7-mixtral: yes
# nous-hermes-2-mixtral-8x7b-dpo: 

SYSTEM_ROLE_TEMPLATE='{
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
    grammar_string: $grammar_string,
    seed: $seed
}'

NO_SYSTEM_ROLE_TEMPLATE='{
    messages: [
      {
	role: "user",
	content: $question
      }
    ],
    mode: $mode,
    temperature: $temperature,
    repetition_penalty: $repetition_penalty,
    penalize_nl: $penalize_nl,
    grammar_string: $grammar_string,
    seed: $seed
}'

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

    # fixme: not all models support the system role in the API, and there's no way to tell afaik
    # workaround: if $USE_SYSTEM_ROLE is non-empty, prepend system_message to question
    if [ "${USE_SYSTEM_ROLE}" == "" ];
    then
	TEMPLATE="${NO_SYSTEM_ROLE_TEMPLATE}"
	question=$(printf "%s\n%s" "${system_message}" "${question}")
	system_mesage="xxx unused"
    else
	TEMPLATE="${SYSTEM_ROLE_TEMPLATE}"
    fi

    #set -x
    # remove leading and trailing whitespace for system_message and question
    # prepare system message and question
    system_message=${system_message##[[:space:]]}
    system_message=${system_message%%[[:space:]]}
    system_message_file=$(mktemp); printf "%s\n" "${system_message%$'\n'}" >> "${system_message_file}"

    question=${question##[[:space:]]}
    question=${question%%[[:space:]]}
    question_file=$(mktemp); printf "%s" "${question}" >> "${question_file}"

    data=$(jq --raw-input --raw-output  --compact-output -n \
	      --arg mode "${mode}" \
	      --arg temperature "${temperature}" \
	      --arg repetition_penalty "${repetition_penalty}" \
	      --arg penalize_nl "${penalize_nl}" \
	      --arg seed "${SEED}" \
	      --rawfile system_message "${system_message_file}" \
	      --rawfile question "${question_file}" \
	      --rawfile grammar_string "${grammar_file}" \
	      "${TEMPLATE}" \
	| jq 'del(.[] | select(. == ""))' \
	) || (s=$?; echo "* $0 FAIL: $s"; exit $s)

    #set -x
    if [ "${VERBOSE}" ]; then
	echo "USE_SYSTEM_ROLE='$USE_SYSTEM_ROLE'"
	printf "%s\n" "${data}" | jq --indent 1 >> /dev/stderr
    fi

    # Invoke the HTTP API endpoint via
    result=$(printf "%s" "${data}" | curl -s "${VIA_API_CHAT_COMPLETIONS_ENDPOINT}" -H 'Content-Type: application/json' -d @-)
    output="$(printf "%s" "${result}" | jq --raw-output '.choices[].message.content')"

    rm -f "${question_file}" || echo "* WARN: unable to remove ${question_file}" >> /dev/stderr
    rm -f "${system_message_file}" || echo "* WARN: unable to remove ${system_message_file}" >> /dev/stderr

    printf "%s\n" "${output}" | via_api_mistral_output_fixup
}

function via_api_model {
    via_api_prompt
}

function get_model_name {
    # curl prints
    # `{"model_name":"LoneStriker_dolphin-2.7-mixtral-8x7b-3.75bpw-h6-exl2","lora_names":[]}`
    # todo:  this correct
    curl -s "${VIA_API_MODEL_INFO_ENDPOINT}" | jq -r .model_name
}

function list_models {
    curl --s "${VIA_API_MODEL_LIST_ENDPOINT}" | jq -r '.model_names[]'
}
