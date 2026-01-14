#!/bin/bash -e

SCRIPT_DIR="$(dirname "$(realpath "${BASH_SOURCE[0]}")")"
HELP_SH='help.sh'

HELP_SH_OPTIONS=""
GIT_DIFF_OPTIONS=""
DIFF_COMMAND=""
DIFF_NAME_STATUS_COMMAND=""
UNSTAGED=""

function usage() {
    local p=$(basename "$0")
    echo "$p: [--quiet] [git diff options] [--] [help.sh options]"
    echo $'- --quiet: suppress introductory message'
    echo $'- any next arguments until `--` are given to `git diff`'
    echo $'- all after a `--` is given to `help`'
    echo $'- See `help-commit -- --help` for LLM options passthrough'
}


# help-commit.sh [git diff options] -- [help.sh options]
while [[ $# -gt 0 ]]; do
    case $1 in
        --help)
            usage
            exit 1
            ;;
        --quiet|-q)
            QUIET=1
            shift
            ;;
        --)
            shift
            # Assign as array and fix users.
            # The array elements each map to CLI parameters
            HELP_SH_OPTIONS=("$@")
            break
            ;;
        *)
            GIT_DIFF_OPTIONS+="${1} "
            shift
            ;;
    esac
done

# Construct prompt
#default_system_message="$(printf "%b" "You are an expert in Linux, Bash, Python, general programming, and related topics.\n")"
#export SYSTEM_MESSAGE="${SYSTEM_MESSAGE:-${default_system_message}}"


# TODO: we may need a better way, for example separately tracking staged and unstaged changes
# simply by doing two llm invocations and teeing the staged commit output to the unstaged
# input.
STAGED_PROMPT="Below is the output of \`git diff --numstat\` and \`git diff\`. Read the output and then briefly output a code fence containing a corresponding \`git commit\` command, using multiple \`-m\` commit messages.\n"
UNSTAGED_PROMPT="Below is the output of \`git diff --numstat\` and \`git diff\`. Read the output and then briefly output a code fence containing a corresponding \`git commit -a\` command, using one or more dash-m commit messages as appropriate for the change.\n"

function get_results {
    if ! git rev-parse --is-inside-work-tree > /dev/null 2>&1; then
        echo "$(basename "$0"): PWD=$PWD is not in a git repository"
        exit 1
    fi
    local options="$1"

    # set globals
    DIFF_NAME_STATUS_COMMAND="git diff --numstat ${options} ${GIT_DIFF_OPTIONS}"
    DIFF_COMMAND="git diff ${options} ${GIT_DIFF_OPTIONS}"

    # Execute commands and capture output
    DIFF_NAME_STATUS_OUTPUT="$(${DIFF_NAME_STATUS_COMMAND})"
    DIFF_OUTPUT="$($DIFF_COMMAND)"
}

get_results

if [ -z "${DIFF_OUTPUT}" ]; then
    get_results --staged
    UNSTAGED=1
else
    UNSTAGED=""
fi

if [ -z "${DIFF_OUTPUT}" ]; then
    echo "No changes seen" >&2
    exit 1
fi

echo "** UNSTAGED=$UNSTAGED" >&2

function sanitize_output {
    local output="$1"
    printf '%s' "$output" | sed -e $'s/```/`_`_`/g'
}

# remove triple-backquote from the diff output since we're enclosing the body in that
diff_output_sanitized="$(sanitize_output "$DIFF_OUTPUT")"
diff_name_status_output_sanitized="$(sanitize_output "$DIFF_NAME_STATUS_OUTPUT")"

TEMPLATE='```sh\n$ %s\n%s\n$ %s\n%s\n```'

# Pipeline to send 'git diff' out to 'help' input with prompt
printf -v INPUT "${TEMPLATE}" \
       "${DIFF_NAME_STATUS_COMMAND}" "${diff_name_status_output_sanitized}" \
       "${DIFF_COMMAND}" "${diff_output_sanitized}"

# Pick prompt
if [ -n "${UNSTAGED}" ]; then
    echo "PROMPT=UNSTAGED_PROMPT UNSTAGED=$UNSTAGED" >> /dev/stderr
    PROMPT="${UNSTAGED_PROMPT}"
else
    echo "PROMPT=STAGED_PROMPT UNSTAGED=$UNSTAGED" >> /dev/stderr
    PROMPT="${STAGED_PROMPT}"    
fi

if [ -z "${QUIET}" ]; then
    printf '🤖 %b' "${PROMPT}\n"
fi

# Pass HELP_SH_OPTIONS as multiple args
# use -e to let llm.sh expand the backslahes and such in the prompt
printf '%s\n' "${INPUT}" | ${HELP_SH} "${HELP_SH_OPTIONS[@]}" -- -e "${PROMPT}"

 
