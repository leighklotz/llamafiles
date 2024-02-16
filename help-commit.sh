#!/bin/bash

export MODEL_TYPE="${MODEL_TYPE:=mixtral}"

if [ "$1" == "--oneline" ] || [ "$1" == "--multiline" ];
then
    MESSAGE_LINE=$(echo "$1" | sed -e 's/^--//')
    shift
else
    MESSAGE_LINE=oneline
fi

function get_results {
    # set globals
    staged=" $1 "
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

PROMPT="Provide a commit message for the changes below with a ${MESSAGE_LINE} \`git commit -am\` command:\n"
GRAMMAR_FILE_FLAG="--grammar-file /home/klotz/wip/llamafiles/git-commit-${MESSAGE_LINE}-grammar.gbnf"

# remove triple-backquote from the diff output since we're enclosing the body in that
diff_output_sanitized="$(printf "%s" "$diff_output" | sed -e 's/```/`_`_`/g')"

# uses current default model, e.g. $MODEL_TYPE
#set -x
printf -v INPUT '```
$ %s
%s
```\n' \
       "${diff_command}" "${diff_output_sanitized}"

# printf "%s\n" "${INPUT}"

printf "%s\n" "${INPUT}" | help.sh ${*} ${GRAMMAR_FILE_FLAG} "${PROMPT}"
