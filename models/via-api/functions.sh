#!/bin/bash

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

function via_api_prompt {
    if [ "${INPUT}" == "" ]; then
	printf -v PROMPT "${QUESTION%$'\n'}"
    else
	printf -v PROMPT "${QUESTION%$'\n'} ${INPUT%$'\n'}"
    fi
}

function via_api_priority {
    true
}


# via_api_perform_inference "instruct" "You are a helpful math bot. Answer the user's questions." "${question}"
# via_api_perform_inference "$MODE" "$SYSTEM_PROMPT" "$QUESTION"
function via_api_perform_inference() {
    local mode="$1" system_message="$2" question="$3"
    #set -x
    data=$(jq -n --arg mode "${mode}" --arg system_message "${system_message}" --arg question "${question}" '{
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
    mode: $mode
  }')
  # printf "DATA=%s\n" "${DATA}"
  result=$(printf "%s" "${data}" | curl -s "${VIA_API_ENDPOINT}" -H 'Content-Type: application/json' -d @-)
  output="$(printf "%s" "${result}" | jq -r '.choices[].message.content')"
  printf "%s\n" "${output}"
}

function via_api_check_server {
    # Check if the process is already running
    if [ -f $PIDFILE ]; then
	PID=$(cat $PIDFILE)
	if ps -p $PID > /dev/null; then
            # echo "Process already running with PID $PID"
            return 0
	else
            # echo "PID $PID is not running"
            rm $PIDFILE || (s=$?; echo "$0: failed to remove $PIDFILE ($s)"; exit $s;)
	fi
    fi

    # echo "* starting server for ${MODEL_FILE}"
    ${LLM_LIB_DIR}/llamafile-0.6.2 --server -m ${LLM_MODELS_DIR}/${MODEL_FILE} --nobrowser --host 127.0.0.1 --gpu auto -ngl 24 --port 5000 &
    echo $! > $PIDFILE
    return 0
}

function via_api_model {
    via_api_check_server
    via_api_prompt
}
