#!/bin/bash

SCRIPT_DIR="$(dirname "$(realpath "${BASH_SOURCE}")")"
HELP_SH="help.sh"

HELP_SH_OPTIONS=""
GIT_DIFF_OPTIONS=""
MESSAGE_LINE=""

function usage() {

    p=$(basename "$0")
    echo "$p: [--oneline|--multiline|--pull-request|--git-commit]* [git diff options] [--] [help.sh options]"
    echo '- specify --oneline or --multiline for message style; (might use GBNF schema).'
    echo '- specify --pull-request or --git-commit, for message style'
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
	--oneline|--multiline|--one-line|--multi-line|--pull-request|--git-commit)
	    MESSAGE_LINE="${MESSAGE_LINE}$(printf "%s" "$1" | sed -E -e 's/-+/ /g')"
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

: "${MESSAGE_LINE=one-line git commit}"

# Set main parameters
default_system_message="$(printf "%b" "You are an expert in Linux, Bash, Python, general programming, and related topics.\n")"
export SYSTEM_MESSAGE="${SYSTEM_MESSAGE:-${default_system_message}}"
PROMPT="Provide a ${MESSAGE_LINE} message for the changes listed in unified \`git diff\` below, in the form of a \`git commit\` command or in the  form of text for a pull request:\n"

if [ -n "${INHIBIT_GRAMMAR}" ];
then	 
    GRAMMAR_FILE_FLAG=""
else
    # this is a bit broken with the CLI laxness and additive uses
    GRAMMAR_FILE_FLAG="--grammar-file ${SCRIPT_DIR}/git-commit-${MESSAGE_LINE}-grammar.gbnf"
fi

function get_results {
    if ! git rev-parse --is-inside-work-tree > /dev/null 2>&1;
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
# Pass along all args still unprocessed
printf "%s\n" "${INPUT}" | ${HELP_SH} ${*} ${GRAMMAR_FILE_FLAG} -e -- "${PROMPT}"
