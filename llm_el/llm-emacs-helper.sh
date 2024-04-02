#!/bin/bash

SCRIPT_DIR=$(dirname $(realpath "${BASH_SOURCE}"))
LLM_SH="${SCRIPT_DIR}/../scripts/llm.sh"
DEBUG=""

# usage: llm-emacs-helper.sh model-type use-case major-mode WORDS WORDS WORDS
# usage: llm-emacs-helper.sh model-type use-case major-mode 'WORDS() `WORDS` WORDS'
# stdin is region of input
# e.g. cat foo.sh | ./llm-emacs-helper.sh ask mixtral bash make this arg parsing better
USE_CASE=$1; shift
MODEL_TYPE=$1; shift

MAJOR_MODE=""
PROMPT=""
RAW_FLAG=""
N_PREDICT=""

case "$USE_CASE" in
    rewrite)
	# args: major_mode prompt*
	MAJOR_MODE=$1; shift; PROMPT="${*}"
	printf -v SYSTEM_MESSAGE "Re-write the following %s according to user instructions:\n" "${MAJOR_MODE}"
	;;
    
    todo)
	# args: major_mode prompt*
	MAJOR_MODE=$1; shift; PROMPT="${*}"
	printf -v SYSTEM_MESSAGE "Re-write the following %s to address the 'todo' items, following user instructions:\n" "${MAJOR_MODE}"
	;;
    
    ask)
	# args: major_mode prompt*
	MAJOR_MODE=$1; shift; PROMPT="${*}"
	printf -v SYSTEM_MESSAGE "Answer the user question about this the following %s:\n" "${MAJOR_MODE}"
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
	# args: n_predict
	N_PREDICT=$1; shift
	printf -v SYSTEM_MESSAGE ""
	printf -v PROMPT ""
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

TEMPFILE=$(mktemp)
cat >> "${TEMPFILE}"

# estimate context length
context_length=$(( $(wc -c < "${TEMPFILE}") / 3 ))
context_length=$((context_length < 2048 ? 2048 : context_length > 32768 ? 32768 : context_length))
#set -x

cat "${TEMPFILE}" | ${LLM_SH} -m ${MODEL_TYPE} ${DEBUG} --context-length "${context_length}" --stdin ${RAW_FLAG} ${N_PREDICT} "${PROMPT}" || (cat "${TEMPFILE}"; exit 1)
rm "${TEMPFILE}"
