#!/bin/bash

# First version, requires API

URL=http://localhost:5000/v1/internal/token-count
text="$(cat $1)"
printf -v json '{ "text": "%s" }' "${text//\"/}"
printf "%d\n" "$((${#json}/4))"
printf "%s\n" "${json}" 
printf "%s\n" "${json}" | curl -s -X POST -H "Content-Type: application/json" -d @- $URL | jq '.length'
