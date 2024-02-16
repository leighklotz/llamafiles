#!/bin/bash

ONE_LINE=1

output="$(git diff --staged)"
staged=" staged "

if [ "${output}" == '' ]; then
    echo "No staged changes, looking for unstaged"
    output="$(git diff)"
    staged=" "
fi

if [ "${output}" == '' ]; then
    echo no changes seen
    exit 1
fi

if [ "$ONE_LINE" ]; then
    PROMPT="Output a single-line \`git commit -am\` line that will commit these${staged}changes:"
    GRAMMAR_FILE_FLAG="--grammar-file /home/klotz/wip/llamafiles/git-commit-oneline-grammar.gbnf"
else
    PROMPT="Output a multi-line \`git commit -am\` line that will commit these${staged}changes:"
    GRAMMAR_FILE_FLAG="--grammar-file /home/klotz/wip/llamafiles/git-commit-multiline-grammar.gbnf"
fi


# uses current default model, e.g. $MODEL_TYPE
#set -x
printf '```git
%s
```\n' "${output}" | help.sh ${*} ${GRAMMAR_FILE_FLAG} "${PROMPT}"
