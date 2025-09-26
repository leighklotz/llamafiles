#!/bin/bash

SCRIPT_DIR="$(dirname "$(realpath "${BASH_SOURCE}")")"

# prompt
FILE_PATH="${SCRIPT_DIR}/logit-probs-printer-prompt.txt"

# Use jq to read the file directly and convert it into a JSON object with "text" as the key
JSON_PAYLOAD=$(jq -Rs '{use_sampler: false, prompt: .}' "$FILE_PATH")

# POST the JSON payload to the endpoint using curl
curl --silent -X POST -H "Content-Type: application/json" -d "$JSON_PAYLOAD" http://127.0.0.1:5000/v1/internal/logits
