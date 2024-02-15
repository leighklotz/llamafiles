#!/bin/bash

staged=""

output="$(git diff)"
if [ "${output}" == '' ]; then
    echo NO OUTPUT LOOKING FOR STAGED
    output="$(git diff --staged)"
    staged=" staged "
fi

if [ "${output}" == '' ]; then
    echo no changes seen
    exit 1
fi

code.sh git printf "%s" "${output}" | help.sh "Output a one-line \`git commit -m\` line that will commit these${staged}changes:"
