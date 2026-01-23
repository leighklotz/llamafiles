#!/bin/bash

# Check if the script is being sourced or directly executed
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
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

: "${TOP_K:=-20}"
: "${TOP_P:-0.95}"
: "${MIN_P:-0.1}"

# --grammar-file support is spotty in non-gguf models so default to off
: "${USE_GRAMMAR:-}"

# auto_max_new_tokens seems to work only with HF loaders
# max_tokens should be set to lower if the model has low context (i.e. 2k)
# but if we do not set it then it defaults to 512, at least for gguf

TEMPLATE_SETTINGS="
    temperature: \$temperature,
    seed: \$seed,
    max_new_tokens: \$n_predict,
    n_predict: \$n_predict"
if [ -n "${OPENAI_API_KEY}" ]; then
    AUTHORIZATION_PARAMS=(-H "Authorization: Bearer ${OPENAI_API_KEY}")
    TEMPLATE_SETTINGS="${TEMPLATE_SETTINGS},
        model: \"gpt-5\""
else
    TEMPLATE_SETTINGS="${TEMPLATE_SETTINGS},
    tools: [],
    builtin_tools: [],
    reasoning_effort: \$reasoning_effort,
    mode: \$inference_mode,
    temp: \$temperature,
    temperature_last: true,
    temperature: \$temperature,
    repetition_penalty: \$repetition_penalty,
    penalize_nl: \$penalize_nl,
    grammar_string: \$grammar_string,
    seed: \$seed,
    repeat_last_n: 64, frequency_penalty: 0.000, presence_penalty: 0.000,
    top_k: \$top_k, tfs_z: 1.000, top_p: \$top_p, min_p: \$min_p, typical_p: 1.000,
    mirostat: 0, 
    n_keep: 1,
    auto_max_new_tokens: true,
    skip_special_tokens: false"
fi

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
# todo: Sampling order appears to be a key differentiator for results from llamafile vs ooba but it's un-investigated

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
    log_debug "prepare_prompt"
    if [ -z "${INPUT}" ]; then
        printf -v PROMPT "%s\n" "${QUESTION%$'\n'}"
    else
        # remove trailing newlines
        read -r -d '' QUESTION <<<"$QUESTION"
        read -r -d '' INPUT <<<"$INPUT"
        PROMPT="${QUESTION}"$'\n'"${INPUT}"
    fi
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
# via_api_perform_inference "$INFERENCE_MODE" "$SYSTEM_MESSAGE" "$QUESTION" "$GRAMMAR_FILE"
# todo: so many files and strings back and forth
function via_api_perform_inference() {
    local inference_mode="$1" system_message="$2" question="$3" grammar_file="$4"
    local temperature="$5" repetition_penalty="$6" penalize_nl="$7" n_predict="$8"

    if [ -z "$grammar_file" ] || [ -z "${USE_GRAMMAR}" ]; then
        grammar_file="/dev/null"
    fi

    if [ -z "$temperature" ]; then
        temperature=null
    fi

    # Not all models support the system role in the API, and there's no way to tell
    # if $USE_SYSTEM_ROLE is empty, prepend system_message to question
    if [ -z "${USE_SYSTEM_ROLE}" ]; then
        TEMPLATE="${NO_SYSTEM_ROLE_TEMPLATE}"
        printf -v question "%s\n%s\n" "${system_message%$'\n'}" "${question}"
        system_message=""
    else
        TEMPLATE="${SYSTEM_ROLE_TEMPLATE}"
    fi

    #set -x
    # remove leading and trailing whitespace for system_message and question
    # prepare system message and question
    if [ -n "${system_message}" ]; then
        system_message=${system_message##[[:space:]]}
        system_message=${system_message%%[[:space:]]}
        system_message_file=$(mktemp_file sysmsg)
        register_temp_file "${system_message_file}"
        printf "%s\n" "${system_message%$'\n'}" >> "${system_message_file}"
    else
        system_message_file="/dev/null"
    fi

    read -r -d '' question <<<"$question"   # removes trailing newlines
    question="$(string_trim "$question")"
    question_file="$(mktemp /tmp/quest.XXXXXX)"
    register_temp_file "${question_file}"
    log_debug "writing question to ${question_file}"
    printf "%s" "$question" >> "${question_file}"

    # Drop empty string, and null parameters. NaN seems to show as null.
    # Seed must be a number
    temperature=${temperature:-NaN}

    local n_predict="${n_predict}"
    if [ -z "$n_predict" ]; then
        n_predict=null
    fi

    data=$(jq --raw-input --raw-output  --compact-output -n \
              --arg inference_mode "${inference_mode}" \
              --arg reasoning_effort "${REASONING_EFFORT}" \
              --argjson temperature ${temperature} \
              --argjson repetition_penalty ${repetition_penalty} \
              --argjson penalize_nl ${penalize_nl} \
              --argjson seed ${SEED:-NaN} \
              --argjson top_k ${TOP_K:-NaN} \
              --argjson top_p ${TOP_P:-NaN} \
              --argjson min_p ${MIN_P:-NaN} \
              --argjson n_predict ${n_predict} \
              --rawfile system_message "${system_message_file}" \
              --rawfile question "${question_file}" \
              --rawfile grammar_string "${grammar_file}" \
              "${TEMPLATE}" \
               | jq 'with_entries(select(.value != null))')


    log_debug "processed jq input"

    if [ -n "${VERBOSE}" ]; then
        log_verbose "USE_SYSTEM_ROLE='$USE_SYSTEM_ROLE'"
        log_verbose "data=$(printf "%s\n" "${data}" | jq --indent 1)"
    fi

    # Invoke via the HTTP API endpoint
    # todo might need to do `set -o pipefail` here.
    #set -x
    result=$(printf "%s" "${data}" | curl -s "${VIA_API_CHAT_COMPLETIONS_ENDPOINT}" -H 'Content-Type: application/json' "${AUTHORIZATION_PARAMS[@]}" -d @-)
    s=$?
    if [ "$s" -ne 0 ]; then
        log_and_exit $s "Inference cannot curl VIA_API_CHAT_COMPLETIONS_ENDPOINT=$VIA_API_CHAT_COMPLETIONS_ENDPOINT"
    fi

    if [ -n "${INFO}" ]; then
       usage_output="$(printf "%s" "${result}" | jq -r '.usage | to_entries[] | "\(.key)=\(.value)"' | tr '\n' ' ')"
       log_info "usage: ${usage_output}"
    fi

    if [ -n "${DEBUG_SHOW_JSON}" ]; then
        log_debug "API response: ${result}"
        ### ✅ 2025-10-01 19:04:08.990Z INFO llm.sh: API response: {"id":"chatcmpl-1759345446829761536","object":"chat.completion","created":1759345446,"model":"gpt-oss-120b-Q4_K_M-00001-of-00002.gguf","choices":[{"index":0,"finish_reason":"stop","message":{"role":"assistant","content":"<|channel|>analysis<|message|>We just need to answer 5.<|end|><|start|>assistant<|channel|>final<|message|>2 + 3 = 5."},"tool_calls":[]}],"usage":{"prompt_tokens":128,"completion_tokens":27,"total_tokens":155}}
    fi
    output="$(printf "%s" "${result}" | jq --raw-output '.choices[].message.content')"
    log_debug "Output: ${output}"
    ### ✅ 2025-10-01 19:04:08.995Z INFO llm.sh: Output: <|channel|>analysis<|message|>We just need to answer 5.<|end|><|start|>assistant<|channel|>final<|message|>2 + 3 = 5.
    s=$?
    # exit if we failed to parse
    if [ "$s" != 0 ]; then
        log_and_exit $s "via api perform_inference cannot parse result=${result}"
    fi

    # TODO: If the API returned a "thinking" response, split it out here
    ###                   <|channel|>analysis<|message|>We just need to answer 5.<|end|><|start|>assistant<|channel|>final<|message|>2 + 3 = 5.

    # TODO: If the API returned a "thinking" response, split it out here
    # EXAMPLE: <|channel|>analysis<|message|>We just need to answer 5.<|end|><|start|>assistant<|channel|>final<|message|>2 + 3 = 5.
    if [[ $output == *$'\xC2\xA0'* || $output == *$'\xE2\x80\xAF'* ]]; then
        log_debug "Replacing gpt‑oss fake spaces with real spaces."
        # Replace every C2 A0 (nbsp) and E2 80 AF (hair‑space) with a normal space
        output="${output//[$'\xC2\xA0\xE2\x80\xAF']/ }"
    fi

    log_debug "Checking for thinking response"
    if [[ "$output" =~ ^\<\|channel\|\>analysis\<\|message\|\>(.*)\<\|end\|\> ]]; then
        thinking="${BASH_REMATCH[1]}"
        log_with_icon "🤔" "$thinking"
    fi
    #   <|start|>assistant<|channel|>final<|message|>…  
    if [[ "$output" =~ \<\|start\|\>assistant\<\|channel\|\>final\<\|message\|\>(.*) ]]; then
        output="${BASH_REMATCH[1]}"
    fi

    # Output if we succeeded
    printf "%s\n" "${output}"
    return $s
}

function get_model_name {
    (curl -s "${VIA_API_MODEL_INFO_ENDPOINT}" "${AUTHORIZATION_PARAMS[@]}" | jq -e -r .model_name 2> /dev/null) | sed -e "s/null/${MODEL_NAME_OVERRIDE:-None}/"
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
