#!/bin/bash

IMG="${1}"

LLAVA=~/wip/llamafiles/models/llava-v1.5-7b-q4.llamafile
${LLAVA} --image "${1}" --temp 0 -ngl 35 --grammar 'root ::= [a-z]+ (" " [a-z]+)+' -n 16 -e -p '### User: The name of this image is ...\n### Assistant:' --silent-prompt 2>/dev/null | \
    sed -e's/ /_/g' -e's/$/.jpg/'
