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
  echo "Usage: $0 use-case [options] input"
  echo "  use-case:  One of: rewrite, ask, write, summarize, complete, todo, -h"
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
REORDER_CODE=0 #Use numeric for boolean flags

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

# Function to calculate the comment prefix based on the major mode.
function calculate_comment_prefix {
    local major_mode="$1"
    case "$major_mode" in
        sh-mode)      echo "# "   ;;
        *lisp*-mode)  echo ";;; " ;;
        python*-mode) echo "# "   ;;
        c-mode)       echo "// "  ;;
        c\+\+-mode)   echo "// "  ;;
        *)            echo "// "  ;;
    esac
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
    # args: major_mode prompt* [--n-predict] [--raw-flag]
    MAJOR_MODE=$1; shift; PROMPT="${*}"
    printf -v SYSTEM_MESSAGE "Complete the %s code:\n%s\n" "${MAJOR_MODE}" "${PROMPT}"
    ;;

  *)
    echo "ERROR: unknown use case $USE_CASE"
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

# Reorder code with comments if the REORDER_CODE flag is set.
if [ "$REORDER_CODE" -eq 1 ]; then
  comment_prefix=$(calculate_comment_prefix "${MAJOR_MODE}")
  printf "%s\n" "${result}" | reorder_code "${comment_prefix}"
else
  printf "%s\n" "${result}"
fi

