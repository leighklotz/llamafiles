#!/bin/bash

SCRIPT_DIR=$(dirname $(realpath "${BASH_SOURCE}"))
HELP_SH="help.sh"

HELP_SH_OPTIONS=""
GIT_DIFF_OPTIONS=""
MESSAGE_LINE="oneline"

function usage() {

    p=$(basename "$0")
    echo "$p: [--oneline|--multiline] [git diff options] -- [help.sh options]"
    echo '- specify --oneline or --multiline first, for commit message style'
    echo '- any next arguments until `--` are given to `git diff`'
    echo '- all after a `--` is given to `help.sh`'
    echo "- to change model, use \`$p -- -m $MODEL_TYPE\` or \`export \$MODEL_TYPE=$MODEL_TYPE\`"
    echo "- See \`git diff\` and \`$HELP_SH\` for options"
}

# help-commit.sh [git diff options] -- [help.sh options]
while [[ $# -gt 0 ]]; do
    case $1 in
	--help)
	    usage
	    exit 1
	    ;;
	--oneline|--multiline|--one-line|--multi-line)
	    MESSAGE_LINE=$(printf "%s" "$1" | sed -E -e 's/-//g')
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

# Set main parameters
export MODEL_TYPE="${MODEL_TYPE:=mixtral}"
default_system_message="$(printf "%b" "You are an expert in Linux, Bash, Python, general programming, and related topics.\n")"
export SYSTEM_MESSAGE="${SYSTEM_MESSAGE:-${default_system_message}}"
printf -v PROMPT 'Write a %s `git commit` command line to commit the changes listed in the following `git diff` output:' "${MESSAGE_LINE}"
GRAMMAR_FILE_FLAG="--grammar-file ${SCRIPT_DIR}/git-commit-${MESSAGE_LINE}-grammar.gbnf"
PROMPT="Provide ${MESSAGE_LINE} git commit message for the changes listed in the \`git diff\` below, in the form of a \`git commit\` command:\n"
GRAMMAR_FILE_FLAG="--grammar-file ${SCRIPT_DIR}/git-commit-${MESSAGE_LINE}-grammar.gbnf"

function get_results {
    if ! git rev-parse --is-inside-work-tree 2>&1> /dev/null;
    then
	echo "$(basename "$0"): PWD=$PWD is not in a git repository"
	exit 1
    fi
    # set globals
    local options="$1"
    DIFF_COMMAND="git diff ${options}${options:+ }${GIT_DIFF_OPTIONS}"
    DIFF_OUTPUT="$($DIFF_COMMAND)"
}

get_results
if [ -z "${DIFF_OUTPUT}" ]; then
    echo "No staged changes, looking for unstaged" >> /dev/stderr
    get_results --staged
fi

if [ -z "${DIFF_OUTPUT}" ]; then
    echo "No changes seen" >> /dev/stderr
    exit 1
fi

# remove triple-backquote from the diff output since we're enclosing the body in that
diff_output_sanitized="$(printf "%s" "$DIFF_OUTPUT" | sed -e 's/```/`_`_`/g')"

TEMPLATE='```sh
$ %s
%s
```\n'

# Pipeline to send 'git diff' out to 'help.sh' input with prompt
printf -v INPUT "${TEMPLATE}" "${DIFF_COMMAND}" "${diff_output_sanitized}"
printf "%s\n" "${INPUT}" | ${HELP_SH} ${*} ${GRAMMAR_FILE_FLAG} -e -- "${PROMPT}"
