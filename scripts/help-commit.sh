#!/bin/bash

SCRIPT_DIR=$(dirname $(realpath "${BASH_SOURCE}"))

export MODEL_TYPE="${MODEL_TYPE:=mixtral}"

MESSAGE_LINE=oneline
if [ "$1" == "--oneline" ] || [ "$1" == "--multiline" ];
then
    MESSAGE_LINE=$(echo "$1" | sed -e 's/^--//')
    shift
fi

PROMPT="Provide ${MESSAGE_LINE} git commit message for the changes listed in the \`git diff\` below, in the form of a \`git commit\` command:\n"
GRAMMAR_FILE_FLAG="--grammar-file ${SCRIPT_DIR}/git-commit-${MESSAGE_LINE}-grammar.gbnf"

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

# remove triple-backquote from the diff output since we're enclosing the body in that
diff_output_sanitized="$(printf "%s" "$diff_output" | sed -e 's/```/`_`_`/g')"

# uses current default model, e.g. $MODEL_TYPE
#set -x
printf -v INPUT '```sh
$ %s
%s
```\n' \
       "${diff_command}" "${diff_output_sanitized}"

# printf "%s\n" "${INPUT}"

#set -x
printf "%s\n" "${INPUT}" | help.sh ${*} ${GRAMMAR_FILE_FLAG} -e -- "${PROMPT}"

# todo: allow specification of both `git diff` options and `llm.sh` options
