#!/bin/bash

SCRIPT_DIR="$(dirname "$(realpath "${BASH_SOURCE[0]}")")"
HELP_SH='help.sh'

GRAMMAR_FILE_FLAG=""
HELP_SH_OPTIONS=""
GIT_DIFF_OPTIONS=""
: "${OUTPUT_TYPE:=\`git commit\` command}"
: "${LINE_TYPE:=one-line}"
LINE_TYPE=""

# INHIBIT_GRAMMAR=1		# if it's not working in the model you use, turn it off

function usage() {
    local p=$(basename "$0")
    echo "$p: [--oneline|--multiline|--pull-request|--git-commit]* [--quiet] [git diff options] [--] [help.sh options]"
    echo '- --oneline or --multiline: for message style; (might use GBNF schema).'
    echo '- --pull-request or --git-commit: for message style'
    echo '- --quiet: suppress introductory message'
    echo '- any next arguments until `--` are given to `git diff`'
    echo '- all after a `--` is given to `help.sh`'
    echo '- to change model, use \`$p -- -m $MODEL_TYPE\` or \`export \$MODEL_TYPE=$MODEL_TYPE\`'
    echo "- See \`git diff\` and \`$HELP_SH\` for options"
    echo '- See \`help-commit -- --help\` for LLM options passthrough'
}


# help-commit.sh [git diff options] -- [help.sh options]
while [[ $# -gt 0 ]]; do
    case $1 in
	--help)
	    usage
	    exit 1
	    ;;
	--one-line|--oneline)
	    LINE_TYPE='one-line'
	    shift
	    ;;
	--multi-line|--multiline)
	    LINE_TYPE='multi-line'
	    shift
	    ;;
	--git-commit)
	    OUTPUT_TYPE="${1//--/} command"
	    shift
	    ;;
	--pull-request)
	    OUTPUT_TYPE="${1//--/} message"
	    shift
	    ;;
	--quiet|-q)
	    QUIET=1
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

printf -v MESSAGE_LINE '%s%s' "${OUTPUT_TYPE}" "${LINE_TYPE:+ with a $LINE_TYPE message}"

# Construct Prompt
# todo: fix the string concat
default_system_message="$(printf "%b" "You are an expert in Linux, Bash, Python, general programming, and related topics.\n")"
export SYSTEM_MESSAGE="${SYSTEM_MESSAGE:-${default_system_message}}"

PROMPT="Describe the changes listed in the unified \`git diff\` below and output a ready-to-execute ${MESSAGE_LINE} for the changes. In your output, pay attention to bash quoting syntax.\n"

if [ -z "${QUIET}" ]; then
    printf "%b\n" "${PROMPT}"
fi

if [ -z "${INHIBIT_GRAMMAR}" ]; then
    fn=${SCRIPT_DIR}/${MESSAGE_LINE// /-}-grammar.gbnf
    if [ -f "${fn}" ]; then
	printf -v GRAMMAR_FILE_FLAG -- "--grammar-file %s" "${fn}"
    else
	[ -n "${VERBOSE}" ] && printf "%s: grammar file not found; skipping: %s\n" "$0" "${fn}" >> /dev/stderr
        true
    fi
fi

function get_results {
    if ! git rev-parse --is-inside-work-tree > /dev/null 2>&1; then
	echo "$(basename "$0"): PWD=$PWD is not in a git repository"
	exit 1
    fi
    local options="$1"
    # set globals
    DIFF_COMMAND="git diff ${options}${options:+ }${GIT_DIFF_OPTIONS}"
    DIFF_OUTPUT="$($DIFF_COMMAND)"
}

get_results
if [ -z "${DIFF_OUTPUT}" ]; then
    echo "No staged changes, looking for unstaged" >&2
    get_results --staged
fi

if [ -z "${DIFF_OUTPUT}" ]; then
    echo "No changes seen" >&2
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
