#!/bin/bash

# First version, requires API

URL=http://localhost:5000/v1/internal/token-count
text="$(cat $1)"
json=$(jq -n --arg text "$text" '{ text: $text }')
curl -s -X POST -H "Content-Type: application/json" -d "$json" $URL | jq '.length'
