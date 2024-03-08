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
MAJOR_MODE=$1; shift
PROMPT="${*}"

case "$USE_CASE" in
    rewrite)
	printf -v SYSTEM_MESSAGE "Re-write the following text from major-mode=%s buffer according to user instructions:\n" "${MAJOR_MODE}"
	;;
    
    ask)
	printf -v SYSTEM_MESSAGE "Answer the user question about this the following text from major-mode=%s buffer:\n" "${MAJOR_MODE}"
	;;

    write)
	printf -v SYSTEM_MESSAGE "Write a response according to user instructions and the following text from major-mode=%s buffer:\n" "${MAJOR_MODE}"
	;;

    summarize)
	printf -v SYSTEM_MESSAGE "Summarize the following text from major-mode=%s buffer:" "${MAJOR_MODE}"
	;;

    *)
	printf -v SYSTEM_MESSAGE "Read this following text from major-mode=%s buffer and respond to this request:" "${MAJOR_MODE}"
	;;
esac

export SYSTEM_MESSAGE

TEMPFILE=$(mktemp)
cat >> "${TEMPFILE}"

# estimate context length
context_length=$(( $(wc -c < "${TEMPFILE}") / 3 ))
context_length=$((context_length < 2048 ? 2048 : context_length > 32768 ? 32768 : context_length))

cat "${TEMPFILE}" | ${LLM_SH} -m ${MODEL_TYPE} ${DEBUG} --context-length "${context_length}" --stdin "${PROMPT}" || (cat "${TEMPFILE}"; exit 1)
rm "${TEMPFILE}"
