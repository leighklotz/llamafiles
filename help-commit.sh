#!/bin/bash

staged=""

output="$(git diff --staged)"
staged="staged"
if [ "${output}" == '' ]; then
    echo "No staged changes, looking for unstaged"
    output="$(git diff)"
    staged=" "
fi

if [ "${output}" == '' ]; then
    echo no changes seen
    exit 1
fi

# uses current default model, e.g. $MODEL_TYPE
code.sh git printf "%s" "${output}" | help.sh "Output a one-line \`git commit -m\` line that will commit these${staged}changes:"
