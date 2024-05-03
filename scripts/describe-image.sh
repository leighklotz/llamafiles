#!/bin/bash

IMG="${1}"

LLAVA=~/wip/llamafiles/models/llava-v1.5-7b-q4.llamafile
${LLAVA} --image "${1}" --temp 0 -ngl 35 -n 10 -e -p '### User: Describe this image\n### Assistant:' --silent-prompt 2>/dev/null 
