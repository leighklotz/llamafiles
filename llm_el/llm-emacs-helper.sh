#!/bin/bash

# Script to interact with an LLM for code/text manipulation via Emacs major mode.
# It accepts a use case, model type, and input (either from stdin or a file) to perform
# tasks like rewriting, summarizing, completing, or answering questions about the input.
# It handles context length estimation and optional code reordering with comments.

SCRIPT_DIR="$(dirname "$(realpath "${BASH_SOURCE[0]}")")"
LLM_SH="${SCRIPT_DIR}/../scripts/llm.sh"
: "${DEBUG:=}"

# Enable debugging output if the DEBUG variable is set.
if [ -n "${DEBUG}" ]; then
    (env; printf "\n%s\n" "${*}")
fi

# Usage: llm-emacs-helper.sh use-case model-type via major-mode [options] WORDS WORDS WORDS
# Usage: llm-emacs-helper.sh use-case model-type via major-mode 'WORDS() `WORDS` WORDS'
# Stdin is the region of input.
# Example: cat foo.sh | ./llm-emacs-helper.sh ask api mixtral bash make this arg parsing better
# Some use cases invert the sense of text and code, changing text to comments and removing code fences.

USE_CASE=$1; shift
VIA=$1; shift
MODEL_TYPE=$1; shift

MAJOR_MODE=""
PROMPT=""
RAW_FLAG=""
N_PREDICT=""
REORDER_CODE=""

# Function to reorder code by adding a comment sequence before each line, except within code fences.
# The default comment sequence is "// ", but can be specified as the first argument.
function reorder_code {
    # Check if a comment sequence is provided, otherwise default to "// "
    comment_seq="${1:-// }"

    gawk -v comment_seq="$comment_seq" '
    BEGIN {
        in_code_fence = 0
        lang = ""
    }

    {
        if ($0 ~ /^```(.*)$/) {
            in_code_fence = 1 - in_code_fence
            if (in_code_fence == 0) {
                lang = ""
            } else {
                lang = gensub(/^```(.*)$/, "\\1", "g")
            }
        } else if (in_code_fence == 0) {
            print comment_seq $0
        } else {
            print $0
        }
    }
    '
}

# Function to calculate the comment prefix based on the major mode.
function calculate_comment_case {
    local major_mode="$1"
    local comment_prefix=""

    case "$major_mode" in
        sh-mode)      comment_prefix="# "   ;;
        *lisp*-mode)  comment_prefix=";;; " ;;
        python*-mode) comment_prefix="# "   ;;
        c-mode)       comment_prefix="// "  ;;
        c\+\+-mode)   comment_prefix="// "  ;;
        *)            comment_prefix="// "  ;;
    esac

    echo "$comment_prefix"
}

# Process the use case and set the system message accordingly.
case "$USE_CASE" in
    -h)
        usage
	    exit 0
	    ;;
    rewrite)
	    # args: major_mode prompt*
	    MAJOR_MODE=$1; shift; PROMPT="${*}"
	    printf -v SYSTEM_MESSAGE "Re-write the following %s according to user instructions:\n" "${MAJOR_MODE}"
        REORDER_CODE=1
	    ;;
    
    todo)
	    # args: major_mode prompt*
	    MAJOR_MODE=$1; shift; PROMPT="${*}"
	    printf -v SYSTEM_MESSAGE "Re-write the following %s to address the 'todo' items, following user instructions:\n" "${MAJOR_MODE}"
        REORDER_CODE=1
	    ;;
    
    ask)
	    # args: major_mode prompt*
	    MAJOR_MODE=$1; shift; PROMPT="${*}"
	    printf -v SYSTEM_MESSAGE "Answer the user question about the following %s content:\n" "${MAJOR_MODE}"
	    ;;

    write)
	    # args: major_mode prompt*
	    MAJOR_MODE=$1; shift; PROMPT="${*}"
	    printf -v SYSTEM_MESSAGE "Write a response according to user instructions and the following %s:\n" "${MAJOR_MODE}"
	    ;;

    summarize)
	    # args: major_mode prompt*
	    MAJOR_MODE=$1; shift; PROMPT="${*}"
	    printf -v SYSTEM_MESSAGE "Summarize the following %s:" "${MAJOR_MODE}"
	    ;;

    complete)
	    # args: major_mode n_predict prompt*
	    N_PREDICT=$1; shift; PROMPT="${*}"
	    export SYSTEM_MESSAGE="Complete the ${MAJOR_MODE} code:"
	    RAW_FLAG="--raw-input"
	    N_PREDICT="--n-predict ${N_PREDICT}"
	    ;;

    *)
	    # args: major_mode prompt*
	    MAJOR_MODE=$1; shift; PROMPT="${*}"
	    printf -v SYSTEM_MESSAGE "Read this following %s and respond to this request:" "${MAJOR_MODE}"
	    ;;
esac

export SYSTEM_MESSAGE

# Create a temporary file to store the input.
INPUT_TEMPFILE=$(mktemp)
cat >> "${INPUT_TEMPFILE}"

# Estimate context length (limited to 2048-32768 characters)
context_length=$(( $(wc -c < "${INPUT_TEMPFILE}") / 3 ))
context_length=$((context_length < 2048 ? 2048 : context_length > 32768 ? 32768 : context_length))

# Execute the LLM script and capture the result.
result="$(cat "${INPUT_TEMPFILE}" | ${LLM_SH} --via "${VIA}" -m "${MODEL_TYPE}" ${DEBUG} --context-length "${context_length}" --stdin ${RAW_FLAG} ${N_PREDICT} "${PROMPT}")"

# If the LLM script returns an empty result, exit with an error.
if [ -z "${result}" ]; then
    cat "${INPUT_TEMPFILE}"
    exit 1
fi

# Reorder code with comments if the REORDER_CODE flag is set.
if [ -n "$REORDER_CODE" ]; then
    comment_prefix=$(calculate_comment_case "${MAJOR_MODE}")
    printf "%s\n" "${result}" | reorder_code "${comment_prefix}"
else
    printf "%s\n" "${result}"
fi

# Clean up the temporary file.
# TODO: Do this with a trap
rm "${INPUT_TEMPFILE}"
