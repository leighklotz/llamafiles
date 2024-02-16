#!/bin/bash

ONE_LINE=1

function get_results {
    # set globals
    staged=" $1 "
    summary_command="git diff --compact-summary $staged"
    summary_output="$($summary_command)"
    diff_command="git diff $staged"
    diff_output="$($diff_command)"
}

get_results --staged

if [ "${diff_output}" == '' ]; then
    echo "No staged changes, looking for unstaged" >> /dev/stderr
    get_results ""
fi

if [ "${diff_output}" == '' ]; then
    echo "No changes seen" >> /dev/stderr
    exit 1
fi

if [ "$ONE_LINE" ]; then
    PROMPT="Based on the git diff and summary results, output a single-line \`git commit -am\` line that will commit these${staged}changes:"
    GRAMMAR_FILE_FLAG="--grammar-file /home/klotz/wip/llamafiles/git-commit-oneline-grammar.gbnf"
else
    PROMPT="on the git diff and summary results, output a multi-line \`git commit -am\` line that will commit these${staged}changes:"
    GRAMMAR_FILE_FLAG="--grammar-file /home/klotz/wip/llamafiles/git-commit-multiline-grammar.gbnf"
fi


# uses current default model, e.g. $MODEL_TYPE
#set -x
printf '```bash
$ %s
%s
$ %s
%s
```\n' "${diff_command}" "${diff_output}" "${summary_command}" "${summary_output}" \
        | help.sh ${*} ${GRAMMAR_FILE_FLAG} "${PROMPT}"
