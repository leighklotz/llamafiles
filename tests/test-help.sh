#!/bin/bash
SCRIPT_DIR=$(dirname $(readlink -f "${BASH_SOURCE}"))

# This script reads lists of questions from files and
# uses the `help.sh` script to test each question.


# Test Configuration
QUESTIONS_FILE=${SCRIPT_DIR}/questions-help.txt

# Read  line  by line and store each line in an array
declare -a questions
IFS=$'\n' read -d '' -ra questions < "${QUESTIONS_FILE}"

for q in "${questions[@]}"
do
    echo "=== ${q} ==="
    time help.sh --fast -m "${m}" "${q}"
    if [ $? -ne 0 ]; then
        echo "FAIL: status=$?"
    fi
    echo ""
done

