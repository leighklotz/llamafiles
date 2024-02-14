#!/bin/bash
SCRIPT_DIR=$(dirname $(readlink -f "${BASH_SOURCE}"))

# This script reads lists of questions and model types from files, and
# uses the `help.sh` script to test each question with each model type.


# Test Configuration
MODEL_TYPES_FILE="${SCRIPT_DIR}/../models/model-types.txt"
QUESTIONS_FILE=${SCRIPT_DIR}/help-txt.questions

# Read  line  by line and store each line in an array
declare -a questions
declare -a model_types
IFS=$'\n' read -ra questions < "${QUESTIONS_FILE}"
IFS=$' ' read -ra model_types < "${MODEL_TYPES_FILE}"

for q in "${questions[@]}"
do
    echo "=== ${q} ==="
    for m in "${model_types[@]}"
    do
	echo -n "${m} "
	time help.sh --speed -m "${m}" "${q}"
	echo ""
    done
    echo ""
done

