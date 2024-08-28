#!/bin/bash

SCRIPT_DIR="$(dirname "$(realpath "${BASH_SOURCE}")")"

# usage
if [ $# -eq 0 ] || [ "$1" == "--help" ]; then
    echo "$0: [man options] -- [help.sh options]"
    echo "You might also just try 'man foo | help.sh question'"
    exit 0
fi

# Extract options before "--"
MAN_OPTIONS=$(echo "$*" | sed -e 's/ --.*//')

# Extract options after "--"
LLM_OPTIONS=$(echo "$*" | sed -e 's/^.*-- //')

#echo "MAN_OPTIONS='$MAN_OPTIONS'"
#echo "LLM_OPTIONS='$LLM_OPTIONS'"

codeblock man $MAN_OPTIONS | "help.sh" ${LLM_OPTIONS}
