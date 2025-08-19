#!/bin/bash

# First version, requires API
URL="http://tensor-psy.klotz.me:5000/v1/internal/token-count"

# Escape double quotes for JSON using jq.  This is safer than relying on shell escaping.
# Send the JSON payload to the API using curl, piping the JSON directly.
response=$(jq --raw-input --slurp '{"text": .}' | curl -s -X POST -H "Content-Type: application/json" --json @- "$URL")

# Check curl's exit status.
if [ $? -ne 0 ]; then
  echo "Error: curl failed to connect to the API." >&2
  echo "Response: $response" >&2 # Print the response for debugging
  exit 1
fi

# Extract the token length from the response using jq.
length=$(echo "$response" | jq -r '.length') #Use -r for raw output, avoiding quotes

# Check if jq successfully extracted the length.
if [[ -z "$length" ]]; then
  echo "Error: Failed to extract 'length' from API response." >&2
  echo "Response: $response" >&2
  exit 1
fi

# Print the token length.
printf "%s\n" "$length"
