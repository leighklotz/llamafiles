#!/bin/bash

SCRIPT_DIR="$(dirname "$(realpath "${BASH_SOURCE[0]}")")"
LLM_SH="${SCRIPT_DIR}/../scripts/llm.sh"
: "${DEBUG:=}"
if [ -n "${DEBUG}" ]; then
    (env; printf "\n%s\n" "${*}")
fi

# usage: llm-emacs-helper.sh use-case model-type via major-mode WORDS WORDS WORDS
# usage: llm-emacs-helper.sh use-case model-type via major-mode 'WORDS() `WORDS` WORDS'
# stdin is region of input
# e.g. cat foo.sh | ./llm-emacs-helper.sh ask api mixtral bash make this arg parsing better
# some use cases invert the sense of text and code, changing text to comments and removing code fences.

USE_CASE=$1; shift
VIA=$1; shift
MODEL_TYPE=$1; shift


MAJOR_MODE=""
PROMPT=""
RAW_FLAG=""
N_PREDICT=""
REORDER_CODE=""

function reorder_code {
    # Check if a comment sequence is provided, otherwise default to "// "
    comment_seq="${1:-// }"

    awk -v comment_seq="$comment_seq" '
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

case "$USE_CASE" in
    -h) usage
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

INPUT_TEMPFILE=$(mktemp)
cat >> "${INPUT_TEMPFILE}"

# estimate context length
context_length=$(( $(wc -c < "${INPUT_TEMPFILE}") / 3 ))
context_length=$((context_length < 2048 ? 2048 : context_length > 32768 ? 32768 : context_length))
#set -x

result="$(cat "${INPUT_TEMPFILE}" | ${LLM_SH} --via "${VIA}" -m "${MODEL_TYPE}" ${DEBUG} --context-length "${context_length}" --stdin ${RAW_FLAG} ${N_PREDICT} "${PROMPT}")"
if [ -z "${result}" ]; then
    cat "${INPUT_TEMPFILE}"
    exit 1
fi

if [ -n "$REORDER_CODE" ]; then
    # todo: move this calculation to emacs since it already has the info
    case "${MAJOR_MODE}" in
        sh-mode) comment="# " ;;
        *lisp*-mode) comment=";;; " ;;
        python*-mode) comment="# " ;;
        c-mode) comment="// " ;;
        c\+\+-mode) comment="// " ;;
        *) comment="// ";;
    esac
    printf "%s\n" "${result}" | reorder_code "${comment}"
else
    printf "%s\n" "${result}"
fi
rm "${INPUT_TEMPFILE}"
