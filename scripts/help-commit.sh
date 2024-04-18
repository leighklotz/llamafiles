#!/bin/bash

SCRIPT_DIR=$(dirname $(realpath "${BASH_SOURCE}"))

HELP_SH_OPTIONS=""
GIT_DIFF_OPTIONS=""
MESSAGE_LINE="oneline"
export MODEL_TYPE="${MODEL_TYPE:=mixtral}"

function usage() {
    echo "help-commit.sh [git diff options] -- [help.sh options]"
}

# help-commit.sh [git diff options] -- [help.sh options]
while [[ $# -gt 0 ]]; do
    case $1 in
	--oneline|--multiline|--one-line|--multi-line)
	    MESSAGE_LINE=$(echo "$1" | sed -E -e 's/-//g')
	    shift
	    ;;
        --)
	    shift
            HELP_SH_OPTIONS="${*}"
            break
            ;;
	*)
            GIT_DIFF_OPTIONS+="${1} "
	    shift
	    ;;
    esac
done

PROMPT='Explain the following git diff as a ${MESSAGE_LINE} git commit message, expressed in the form of a \`git commit\` command.\n'
GRAMMAR_FILE_FLAG="--grammar-file ${SCRIPT_DIR}/git-commit-${MESSAGE_LINE}-grammar.gbnf"

function get_results {
    # set globals
    staged=" $1 "
    diff_command="git diff $staged ${GIT_DIFF_OPTIONS}"
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
