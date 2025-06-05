#!/bin/bash

# Check if the script is being sourced or directly executed
if [[ "${BASH_SOURCE[0]}" == "${0}" ]];
then
    echo "This script '${BASH_SOURCE[0]}' is intended to be sourced, not executed directly."
#    exit 1
fi

: "${VIA_API_CHAT_BASE:=http://localhost:5000}"
VIA_API_CHAT_COMPLETIONS_ENDPOINT="${VIA_API_CHAT_BASE}/v1/chat/completions"
VIA_API_TOKEN_COUNT_ENDPOINT="${VIA_API_CHAT_BASE}/v1/chat/token-count"
VIA_API_MODEL_INFO_ENDPOINT="${VIA_API_CHAT_BASE}/v1/internal/model/info"
VIA_API_MODEL_LIST_ENDPOINT="${VIA_API_CHAT_BASE}/v1/internal/model/list"
VIA_API_LOAD_MODEL_ENDPOINT="${VIA_API_CHAT_BASE}/v1/internal/model/load"
VIA_API_UNLOAD_MODEL_ENDPOINT="${VIA_API_CHAT_BASE}/v1/internal/model/unload"
AUTHORIZATION_PARAMS=()

# LLM_LIB_DIR="$(realpath "${SCRIPT_DIR}/../lib")"
# LLM_MODELS_DIR="$(realpath "${SCRIPT_DIR}/../models")"
: "{SEED:=NaN}"

# TODO: sampling order:  CFG -> Penalties -> top_k -> tfs_z -> typical_p -> top_p -> min_p -> temperature
# todo: Sampling order appears to be a key differentiator for results from llamafile vs ooba but it's ununnvestigagted
TOP_K="${TOP_K:-20}"
TOP_P="${TOP_P:-0.95}"
MIN_P="${MIN_P:-0.1}"

# --grammar-file support is spotty in non-gguf models so default to off
: "${VIA_API_USE_GRAMMAR:=}"

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

# auto_max_new_tokens seems to work only with HF loaders
# max_tokens should be set to lower if the model has low context (i.e. 2k)
# but if we do not set it then it defaults to 512, at least for gguf

TEMPLATE_SETTINGS="
    temperature: \$temperature,
    seed: \$seed,
    max_tokens: 4096"
if [ -n "${OPENAI_API_KEY}" ]; then
    AUTHORIZATION_PARAMS=(-H "Authorization: Bearer ${OPENAI_API_KEY}")
else
    TEMPLATE_SETTINGS="${TEMPLATE_SETTINGS},
    mode: \$inference_mode,
    temp: \$temperature,
    temperature_last: true,
    temperature: \$temperature,
    repetition_penalty: \$repetition_penalty,
    penalize_nl: \$penalize_nl,
    grammar_string: \$grammar_string,
    seed: \$seed,
    repetition_penalty: \$repetition_penalty,
    repeat_last_n: 64, repeat_penalty: 1.000, frequency_penalty: 0.000, presence_penalty: 0.000,
    top_k: ${TOP_K}, tfs_z: 1.000, top_p: ${TOP_P}, min_p: ${MIN_P}, typical_p: 1.000,
    mirostat: 0, mirostat_lr: 0.100, mirostat_ent: 5.000,
    n_keep: 1,
    auto_max_new_tokens: true,
    max_new_tokens: 4096,
    max_tokens: 4096,
    skip_special_tokens: false"
fi

SYSTEM_ROLE_TEMPLATE="{
    model: \$model_name,
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
    model: \$model_name,
    messages: [
      {
        role: \"user\",
        content: \$question
      }
    ],
    ${TEMPLATE_SETTINGS}    
}"

function prepare_prompt {
    log_info "prepare_prompt"
    if [ -z "${INPUT}" ];
    then
        printf -v PROMPT "%s\n" "${QUESTION%$'\n'}"
    else
        # remove trailing newlines
        read -r -d '' QUESTION <<<"$QUESTION"
        read -r -d '' INPUT <<<"$INPUT"
        PROMPT="${QUESTION}"$'\n'"${INPUT}"
    fi
}

# fixme: does not accept options yet
# fixme: other env are pre-calculated; try to move them here
# fixme: these are fixed and not variable
# fixme: these parameters are set in model loading and cannot be accomodated here
# ${N_PREDICT} ${BATCH_SIZE}
# fixme: what do do about this parameter for API-bound fields?
# ${LLM_ADDITIONAL_ARGS}
function via_set_options {
    repeat_penalty="1"
    penalize_nl="false"
    MODEL_MODE="${MODEL_MODE:-instruct}"
}


# todo: make this be run only on mixtral, use as test case to separate api impl from model quirks
# fix mistral "\_" and "\*"
# https://www.reddit.com/r/LocalLLaMA/comments/1agrddy/has_anyone_encountered_mistrals_tendency_to_use/
function via_api_mistral_output_fixup {
    sed -e 's/\\_/_/g' | sed -e 's/\\\*/*/g'
}

function string_trim() {
    local s="$1"
    # Remove leading whitespace
    s="${s#"${s%%[![:space:]]*}"}"
    # Remove trailing whitespace
    s="${s%"${s##*[![:space:]]}"}"
    printf '%s' "$s"
}

# todo: make common with cli_perform_inference by splitting out all
#       non-inference settings to the prepare_model
# via_api_perform_inference "$MODEL_TYPE" "$INFERENCE_MODE" "$SYSTEM_PROMPT" "$QUESTION" "$GRAMMAR_FILE"
# todo: so many files and strings back and forth
function via_api_perform_inference() {
    local model_type="$1" inference_mode="$2" system_message="$3" question="$4" grammar_file="$5"
    local temperature="$6" repetition_penalty="$7" penalize_nl="$8"

    if [ -z "$grammar_file" ] || [ -z "${VIA_API_USE_GRAMMAR}" ];
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
        #question=$(printf "%s\n%s\n" "${system_message%$'\n'}" "${question}")
        printf -v question "%s\n%s\n" "${system_message%$'\n'}" "${question}"
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
        system_message_file=$(mktemp_file sysmsg);
        register_temp_file "${system_message_file}"
        printf "%s\n" "${system_message%$'\n'}" >> "${system_message_file}"
    else
        system_message_file="/dev/null"
    fi

    read -r -d '' question <<<"$question"   # removes trailing newlines
    question="$(string_trim "$question")"
    question_file="$(mktemp /tmp/quest.XXXXXX)"
    TEMP_FILES+=("$question_file")
    log_info "writing question to ${question_file}"
    printf "%s" "$question" >> "${question_file}"

    # hack: Drop empty string, and null parameters. NaN seems th show as null.
    #       sadly seed must be a number
    temperature=${temperature:-NaN} 
    data=$(jq --raw-input --raw-output  --compact-output -n \
              --arg inference_mode "${inference_mode}" \
              --argjson temperature ${temperature} \
              --argjson repetition_penalty ${repetition_penalty} \
              --argjson penalize_nl ${penalize_nl} \
              --argjson seed ${SEED} \
              --arg model_name "${model_type}" \
              --rawfile system_message "${system_message_file}" \
              --rawfile question "${question_file}" \
              --rawfile grammar_string "${grammar_file}" \
              "${TEMPLATE}" \
           | jq 'with_entries(select(.value != null))' \
        )
    
    log_info "processed jq input"

    if [ -n "${VERBOSE}" ];
    then
        log_verbose "USE_SYSTEM_ROLE='$USE_SYSTEM_ROLE'"
        log_verbose "data=$(printf "%s\n" "${data}" | jq --indent 1)"
    fi

    # Invoke via the HTTP API endpoint
    # todo might need to do `set -o pipefail` here.
    #set -x
    result=$(printf "%s" "${data}" | curl -s "${VIA_API_CHAT_COMPLETIONS_ENDPOINT}" -H 'Content-Type: application/json' "${AUTHORIZATION_PARAMS[@]}" -d @-)
    s=$?
    if [ "$s" -ne 0 ];
    then
        log_and_exit $s "via --api perform inference cannot curl"
    fi

    if [ -n "${INFO}" ]; then
       usage_output="$(printf "%s" "${result}" | jq -r '.usage | to_entries[] | "\(.key)=\(.value)"' | tr '\n' ' ')"
       log_info "usage: ${usage_output}"
    fi

    output="$(printf "%s" "${result}" | jq --raw-output '.choices[].message.content')"
    s=$?

    # exit if we failed to parse
    if [ "$s" != 0 ]; then
        log_and_exit $s "via api perform_inference cannot parse ${result}"
    fi

    # Output if we succeeded
    printf "%s\n" "${output}" | via_api_mistral_output_fixup
    return $s
}


# can't set the model in the API so we just validate that
# there is a model. 
# todo: maybe give error if $model_name != $MODEL_NAME and MODEL_NAME is specified.
function set_model_name {
    model_name="$(get_model_name)"
    if [ "$model_name" == "None" ];
    then
        log_and_exit 2 "No model loaded via --api"
    fi
    log_info "Setting model_name to ${model_name}"
}

function init_via_model {
    # could verify that there is a model loaded
    true
}

function prepare_model {
    set_model_name
    prepare_prompt
}

function get_model_name {
    # curl prints
    # `{"model_name":"LoneStriker_dolphin-2.7-mixtral-8x7b-3.75bpw-h6-exl2","lora_names":[]}`
    # set -euo pipefail
    (curl -s "${VIA_API_MODEL_INFO_ENDPOINT}" "${AUTHORIZATION_PARAMS[@]}" | jq -e -r .model_name 2> /dev/null) || echo "gpt"
}

function list_models {
    local args=$@

    if [ -z "$args" ]; then
        curl -s "${VIA_API_MODEL_LIST_ENDPOINT}" "${AUTHORIZATION_PARAMS[@]}" | jq -r '.model_names[]'
    else
        # Quote the arguments to prevent word splitting issues, and handle empty arguments.
        local quoted_args=("$@")

        # Build the grep pattern.  We'll use xargs to pass multiple -e options to grep.
        local grep_pattern=""
        for arg in "${quoted_args[@]}"; do
            if [ -n "$arg" ]; then #Skip empty arguments
                grep_pattern="$grep_pattern -e $arg"
            fi
        done

        list_models | grep -i $grep_pattern
    fi
}

function list_model_types() {
    echo any
}

## fixme: overloads models/function.sh, which is really via/llamafile/function.sh
## and this is really via/api/function.sh.
function load_model {
    local model_path="$1"
    local data
    printf -v data '{ "model_name": "%s", "settings": {}, "args": {} }' "${model_path}"
    # use no quotes on AUTHORIZATION_PARAMS so it expands into nothing if unset, or multiple tokens if set
    result=$(printf "%s" "${data}" | curl -s "${VIA_API_LOAD_MODEL_ENDPOINT}" -H 'Content-Type: application/json' ${AUTHORIZATION_PARAMS[0]} -d @- || { log_and_exit $? "via --api --load-model cannot curl"; return 1; })
    [ -n "${INFO}" ] && log_info "load_model result= ${result}"
    grep -s "OK" <<< "${result}"
    local s=$?
    [ $s -ne 0 ] && log_error "load_model result=${result}"
    return $s;
}

function unload_model {
    result=$(printf "%s" "${data}" | curl -s "${VIA_API_UNLOAD_MODEL_ENDPOINT}" "${AUTHORIZATION_PARAMS[@]}" -d '' || log_and_exit $? "via --api --unload-model cannot curl")
    printf "%s\n" "$result"
}

# todo: support via=api; use cli or api calls for accurate counts;
function truncate_to_context_length {
    true
}
