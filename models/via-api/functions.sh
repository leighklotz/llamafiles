#!/bin/bash

VIA_API_CHAT_BASE="${VIA_API_CHAT_BASE:-http://localhost:5000}"
VIA_API_CHAT_COMPLETIONS_ENDPOINT="${VIA_API_CHAT_BASE}/v1/chat/completions"
VIA_API_MODEL_INFO_ENDPOINT="${VIA_API_CHAT_BASE}/v1/internal/model/info"
VIA_API_MODEL_LIST_ENDPOINT="${VIA_API_CHAT_BASE}/v1/internal/model/list"
VIA_API_LOAD_MODEL_ENDPOINT="${VIA_API_CHAT_BASE}/v1/internal/model/load"
VIA_API_UNLOAD_MODEL_ENDPOINT="${VIA_API_CHAT_BASE}/v1/internal/model/unload"
VIA_API_INHIBIT_GRAMMAR="${VIA_API_INHIBIT_GRAMMAR:-}"

# Check if the script is being sourced or directly executed
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    echo "This script '${BASH_SOURCE[0]}' is intended to be sourced, not executed directly."
    exit 1
fi

LLM_LIB_DIR=$(realpath "${SCRIPT_DIR}/../lib")
LLM_MODELS_DIR=$(realpath "${SCRIPT_DIR}/../models")
MODEL_FILE="mixtral/mixtral-8x7b-instruct-v0.1.Q5_K_M.llamafile"
PIDFILE="/tmp/via-api.pid"
SEED="${SEED:-NaN}"

# fixme: some models support the system role API and some do not.
# looks like MODEL_MODE must be "instruct" to use
# system-message; otherwise it's a mix of context, characters,
# and presets, best avoided for now.
#
# todo: query ooba API to find the model behind the api,
# and add put a new function to each model/*/*functions.sh
# to determinue USE_SYSTEM_ROLE properly for each model type.
#
# workaround: for now use `export USE_SYSTEM_ROLE=1` if you need it.
# mixtral-7b-instruct-v0.1: no
# dolphin-2.6-mistral-7b-dpo: yes
# dolphin-2.7-mixtral: yes
USE_SYSTEM_ROLE="${USE_SYSTEM_ROLE:-}"

TEMPLATE_SETTINGS="
    mode: \$mode,
    temperature_last: true,
    temperature: \$temperature,
    repetition_penalty: \$repetition_penalty,
    penalize_nl: \$penalize_nl,
    grammar_string: \$grammar_string,
    seed: \$seed,
    repeat_last_n: 64, repeat_penalty: 1.000, frequency_penalty: 0.000, presence_penalty: 0.000,
    top_k: 40, tfs_z: 1.000, top_p: 0.950, min_p: 0.050, typical_p: 1.000, temp: 0.000,
    mirostat: 0, mirostat_lr: 0.100, mirostat_ent: 5.000,
    n_keep: 1"

SYSTEM_ROLE_TEMPLATE="{
    messages: [
      {
	role: \"system\",
	content: \$system_message
      },
      {
	role: \"user\",
	content: \$question
      }
    ],
    ${TEMPLATE_SETTINGS}
}"

# sampling order:  CFG -> Penalties -> top_k -> tfs_z -> typical_p -> top_p -> min_p -> temperature 

NO_SYSTEM_ROLE_TEMPLATE="{
    messages: [
      {
	role: \"user\",
	content: \$question
      }
    ],
    ${TEMPLATE_SETTINGS}    
}"

function prepare_prompt {
    if [ "${INPUT}" == "" ]; then
	printf -v PROMPT "%s" "${QUESTION%$'\n'}"
    else
	printf -v PROMPT "%s\\nn%s" "${QUESTION%$'\n'}" "${INPUT%$'\n'}"
    fi
}

function prepare_priority {
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

    if [ -z "$grammar_file" ] || [ -n "${VIA_API_INHIBIT_GRAMMAR}" ];
    then
	grammar_file="/dev/null"
    fi

    if [ -z "$temperature" ];
    then
	temperature=null
    fi

    # fixme: not all models support the system role in the API, and there's no way to tell afaik
    # workaround: if $USE_SYSTEM_ROLE is non-empty, prepend system_message to question
    if [ -z "${USE_SYSTEM_ROLE}" ];
    then
	TEMPLATE="${NO_SYSTEM_ROLE_TEMPLATE}"
	question=$(printf "%s\n%s" "${system_message}" "${question}")
	system_message=""
    else
	TEMPLATE="${SYSTEM_ROLE_TEMPLATE}"
    fi

    #set -x
    # remove leading and trailing whitespace for system_message and question
    # prepare system message and question
    if [ -n "${system_message}" ];
    then
	system_message=${system_message##[[:space:]]}
	system_message=${system_message%%[[:space:]]}
	system_message_file=$(mktemp -t sysmsg.XXXXXX); printf "%s\n" "${system_message%$'\n'}" >> "${system_message_file}"
    else
	system_message_file="/dev/null"
    fi

    question=${question##[[:space:]]}
    question=${question%%[[:space:]]}
    question_file=$(mktemp -t quest.XXXXXX); printf "%s" "${question}" >> "${question_file}"

    # hack: Drop empty string, and null parameters. NaN seems th show as null.
    #       sadly seed must be a number
    temperature=${temperature:-NaN} 
    # set -x
    data=$(jq --raw-input --raw-output  --compact-output -n \
	      --arg mode "${mode}" \
	      --argjson temperature ${temperature} \
	      --argjson repetition_penalty ${repetition_penalty} \
	      --argjson penalize_nl ${penalize_nl} \
	      --argjson seed ${SEED} \
	      --rawfile system_message "${system_message_file}" \
	      --rawfile question "${question_file}" \
	      --rawfile grammar_string "${grammar_file}" \
	      "${TEMPLATE}" \
	| jq 'del(.[] | select(. == ""))' \
	| jq 'del(.[] | select(. == null))' 
	)

    if [ "${VERBOSE}" ]; then
	echo "USE_SYSTEM_ROLE='$USE_SYSTEM_ROLE'"
	printf "%s\n" "${data}" | jq --indent 1 >> /dev/stderr
    fi

    # Invoke via the HTTP API endpoint
    result=$(printf "%s" "${data}" | curl -s "${VIA_API_CHAT_COMPLETIONS_ENDPOINT}" -H 'Content-Type: application/json' -d @-)
    s=$?
    if [ "$s" != 0 ];
    then
	log_warn $s "via-api perform inference cannot curl"
    fi
    output="$(printf "%s" "${result}" | jq --raw-output '.choices[].message.content')"
    s=$?
    if [ "$s" != 0 ];
    then
	log_warn $s "via-api perform inference cannot parse output"
    fi
    case "$KEEP_PROMPT_TEMP_FILE" in
	ALL)
	    true
	    ;;
	ERROR|ERRORS|NONE)
	    if [ "$s" == 0 ] || [ "${KEEP_PROMPT_TEMP_FILE}" == "NONE" ];
	       then
		   [ -n "${question_file}" ] && rm -f "${question_file}" || echo "* WARN: unable to remove ${question_file}" >> /dev/stderr
		   [ -n "${system_message_file}" ] && [ "${system_message_file}" != "/dev/null" ] && rm -f "${system_message_file}" || log_warn $? "* WARN: unable to remove ${system_message_file}"
	    fi
	    ;;
    esac
    printf "%s\n" "${output}" | via_api_mistral_output_fixup
    return $s
}

function prepare_model {
    model_name="$(get_model_name)"
    if [ "$model_name" == "None" ];
    then
	log_and_exit 2 "No model loaded via-api"
    fi
    prepare_prompt
}

function get_model_name {
    # curl prints
    # `{"model_name":"LoneStriker_dolphin-2.7-mixtral-8x7b-3.75bpw-h6-exl2","lora_names":[]}`
    curl -s "${VIA_API_MODEL_INFO_ENDPOINT}" | jq -r .model_name
}

function list_models {
    curl -s "${VIA_API_MODEL_LIST_ENDPOINT}" | jq -r '.model_names[]'
}

function load_model {
    local model_path="$1"
    printf -v data '{ "model_name": "%s", "settings": {}, "args": {} }' "${model_path}"
    result=$(printf "%s" "${data}" | curl -s "${VIA_API_LOAD_MODEL_ENDPOINT}" -H 'Content-Type: application/json' -d @- || log_and_exit $? "via-api load_model cannot curl")
    printf "%s\n" "$result"
}

function unload_model {
    result=$(printf "%s" "${data}" | curl -s "${VIA_API_UNLOAD_MODEL_ENDPOINT}" -d '' || log_and_exit $? "via-api unload_model cannot curl")
    printf "%s\n" "$result"
}
