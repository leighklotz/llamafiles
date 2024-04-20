#!/bin/bash

SCRIPT_DIR=$(dirname $(realpath "${BASH_SOURCE}"))

HELP_SH_OPTIONS=""
GIT_DIFF_OPTIONS=""
MESSAGE_LINE="oneline"
export MODEL_TYPE="${MODEL_TYPE:=mixtral}"

function usage() {
    p=$(basename "$0")
    echo "$p: [git diff options] -- [help.sh options]"
    echo "- change model with \`$p -- -m model\` or \`export \$MODEL_TYPE=$MODEL_TYPE\`"
    #set -x
}

# help-commit.sh [git diff options] -- [help.sh options]
while [[ $# -gt 0 ]]; do
    case $1 in
	--help)
	    usage
	    exit 0
	    ;;
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

# Set SYSTEM_MESSAGE, PROMPT and GRAMMAR_FILE
default_system_message="$(printf "%b" "You are an expert in Linux, Bash, Python, general programming, and related topics.\n")"
export SYSTEM_MESSAGE="${SYSTEM_MESSAGE:-${default_system_message}}"

printf -v PROMPT 'Explain the following git diff as a %s git commit message, expressed in the form of a \`git commit\` command.\n' "${MESSAGE_LINE}"

GRAMMAR_FILE_FLAG="--grammar-file ${SCRIPT_DIR}/git-commit-${MESSAGE_LINE}-grammar.gbnf"

# Pipeline to connect 'git diff' with 'help.sh' below.

function get_results {
    # set globals
    local options=" $1 "
    DIFF_COMMAND="git diff $options ${GIT_DIFF_OPTIONS}"
    DIFF_OUTPUT="$($DIFF_COMMAND)"
}

get_results ""

if [ "${DIFF_OUTPUT}" == '' ]; then
    echo "No staged changes, looking for unstaged" >> /dev/stderr
    get_results --staged
fi

if [ "${DIFF_OUTPUT}" == '' ]; then
    echo "No changes seen" >> /dev/stderr
    exit 1
fi

# remove triple-backquote from the diff output since we're enclosing the body in that
diff_output_sanitized="$(printf "%s" "$DIFF_OUTPUT" | sed -e 's/```/`_`_`/g')"

#set -x
printf -v INPUT '```sh
$ %s
%s
```\n' \
       "${DIFF_COMMAND}" "${diff_output_sanitized}"

# printf "%s\n" "${INPUT}"

#set -x
printf "%s\n" "${INPUT}" | help.sh ${*} ${GRAMMAR_FILE_FLAG} -e -- "${PROMPT}"

# todo: allow specification of both `git diff` options and `llm.sh` options
