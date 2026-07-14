#!/bin/bash

# Script to interface with an LLM from Emacs major modes for code/text manipulation.
# Handles use cases like rewriting, summarizing, completion, and question answering.
#
# Copyright (C) 2024-2025 Leigh L. Klotz, Jr.
# Licensed under the GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.
# See LICENSE file for details.

SCRIPT_DIR="$(dirname "$(realpath "${BASH_SOURCE[0]}")")"
LLM_SH="${SCRIPT_DIR}/../scripts/llm.sh"
: "${DEBUG:=}"

# Enable debugging output if the DEBUG variable is set.
if [ -n "${DEBUG}" ]; then
    (env; printf "\n%s\n" "${*}")
fi

# Usage: llm-emacs-helper.sh use-case [options] input
# Input can be stdin or a file piped to this script.
# Example: cat foo.sh | ./llm-emacs-helper.sh ask api mixtral bash "improve this code"
function usage {
  echo "Usage: $0 -h | use-case [options] input"
  echo "  use-case:  One of: rewrite, ask, write, summarize, complete, todo"
  echo "  options:   -n_predict <tokens> - --raw-input"
  echo "  input:     The text or code to process (piped or file)."
}

# Argument parsing with more robust error handling
if [[ $# -lt 3 ]]; then
  usage
  exit 1
fi

USE_CASE="$1"; shift

MAJOR_MODE=""
PROMPT=""
RAW_FLAG=""
N_PREDICT=""
REORDER_CODE=""
UNFENCE_CODE=""

# Parse optional arguments
while [[ $# -gt 0 ]]; do
  case "$1" in
    -n_predict)
      N_PREDICT="--n-predict \"${2}\""
      shift 2
      ;;
    --raw-input)
      RAW_FLAG="--raw-input"
      shift 1
      ;;
    *)
      break
      ;;
  esac
done

# Function to reorder code by adding a comment sequence before each line, except within code fences.
# This now uses a more concise and readable awk script.
function reorder_code {
  local comment_seq="$1"
  awk -v comment_seq="$comment_seq" '
    {
      if ($0 ~ /^```/) {
        in_code_fence = !in_code_fence
      }
      if (!in_code_fence) {
        print comment_seq, $0
      } else {
        print $0
      }
    }
  '
}

# Function to unfence code and ignore the rest
function unfence_code {
    awk '/^```.*$/ { flag = 1; next } /^```$/ { flag = 0; next } flag { print }'
}

# Function to calculate the comment prefix based on the major mode.
function calculate_comment_prefix {
    local major_mode="$1"
    case "$major_mode" in
        sh-mode)      echo "# "   ;;
        *lisp*-mode)  echo ";;; " ;;
        python*-mode) echo "# "   ;;
        c-mode)       echo "// "  ;;
        c\+\+-mode)   echo "// "  ;;
        markdown-mode) echo "" ;;
        *) echo "🤖" ;;
    esac
}

# Function to calculate the language name based on the major mode.
function calculate_mode_lang {
    local major_mode="$1"
    case "$major_mode" in
        sh-mode)      echo "Shell"   ;;
        emacs-lisp-mode)  echo "Emacs Lisp" ;;
        *lisp*-mode)  echo "Lisp" ;;
        python*-mode) echo "Python"   ;;
        c-mode)       echo "C"  ;;
        c\+\+-mode)   echo "C+"  ;;
        *)            echo "${major_mode/-mode/}"  ;;
    esac
}

# Process the use case and set the system message accordingly.
MAJOR_MODE=$1; shift
MODE_LANG="$(calculate_mode_lang "${MODE_LANG}")"

case "$USE_CASE" in
  -h)
    usage
    exit 0
    ;;
  rewrite)
    # args: major_mode prompt*
    PROMPT="${*}"
    printf -v SYSTEM_MESSAGE 'Re-write the following Emacs `%s` buffer contents according to user instructions. Do not make unrelated changes.\n' "${MODE_LANG}"
    REORDER_CODE=""
    ;;

  todo)
    # args: major_mode prompt*
    PROMPT="${*}"
    printf -v SYSTEM_MESSAGE 'Re-write the following Emacs `%s` to address the 'todo' items, following user instructions. Do not make unrelated changes.\n' "${MODE_LANG}"
    REORDER_CODE=1
    ;;

  ask)
    # args: major_mode prompt*
    PROMPT="${*}"
    printf -v SYSTEM_MESSAGE 'Answer the user question about the following Emacs `%s` buffer content:\n' "${MODE_LANG}"
    ;;

  write)
    # args: major_mode prompt*
    PROMPT="${*}"
    printf -v SYSTEM_MESSAGE 'Write a response according to user instructions and the following Emacs `%s` buffer contents:\n' "${MODE_LANG}"
    ;;

  vibe-emacs)
    # args: major_mode prompt*
    PROMPT="${*}"; UNFENCE_CODE=1
    printf -v SYSTEM_MESSAGE 'Write and output an Emacs Lisp S-expression response that will carry out the following user instructions on the Emacs `%s` mode buffer. Use Emacs Lisp comments for any text that is not the code to execute.\n' "${MODE_LANG}"
    ;;

  summarize)
    # args: major_mode prompt*
    PROMPT="${*}"
    printf -v SYSTEM_MESSAGE 'Summarize the following Emacs `%s` buffer text:' "${MODE_LANG}"
    ;;

  complete)
    # args: major_mode prompt* [--n-predict] [--raw-flag]
    PROMPT="${*}"
    printf -v SYSTEM_MESSAGE 'Complete the Emacs buffer `%s` code:\n%s\n' "${MODE_LANG}" "${PROMPT}"
    ;;

  *)
    printf 'ERROR: MAJOR_MODE=%s; MODEL_LANG=%s; unknown use case=%s\n' "$1" "${MODE_LANG}" "${USE_CASE}"
    exit 1
    ;;
esac

export SYSTEM_MESSAGE

# Create a temporary file to store the input.
INPUT_TEMPFILE=$(mktemp)
trap "rm \"${INPUT_TEMPFILE}\"" EXIT
cat > "${INPUT_TEMPFILE}"

# Execute the LLM script and capture the result.
result="$(cat "${INPUT_TEMPFILE}" | ${LLM_SH} ${DEBUG} --stdin ${RAW_FLAG} ${N_PREDICT} "${PROMPT}")"

# If the LLM script returns an empty result, exit with an error.
if [ -z "${result}" ]; then
    cat "${INPUT_TEMPFILE}"
    exit 1
fi

# Reorder code with comments if theR EORDER_CODE flag is set.
if [ -n "$REORDER_CODE" ]; then
  comment_prefix=$(calculate_comment_prefix "${MAJOR_MODE}")
  # printf "before reorder: result=%s\n" "${result}"
  printf "%s\n" "${result}" | reorder_code "${comment_prefix}"
elif [ -n "$UNFENCE_CODE" ]; then
  printf "%s\n" "${result}" | unfence_code
else
  printf "%s\n" "${result}"
fi

