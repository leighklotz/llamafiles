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
printf '```git
%s
```\n' "${output}" | help.sh ${*} --grammar-file ~/wip/llamafiles/git-commit-grammar.gbnf "Output a one-line \`git commit -am\` line that will commit these${staged}changes:"
