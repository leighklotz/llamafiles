#!/bin/bash -x

SCRIPT_DIR="$(dirname "$(realpath "${BASH_SOURCE}")")"

USAGE="${0}"
QUERY="Write a MicroPython script to read human input of International Morse Code from GPIO10 and output it to Serial one character at a time."

export VIA='api'

# Get site variables from env.sh, if present
[ -f "${SCRIPT_DIR}/env.sh" ] && source "${SCRIPT_DIR}/env.sh"

for model in $(via --list-models);
do
    via --unload-model
    printf "* After unload model=%s nvfree=%s\n" "${model}" "$(nvfree)"
    via --load-model "${model}"
    printf "* After load model=%s nvfree=%s\n" "${model}" "$(nvfree)"    
    printf "\\--------- %s ----------\n" "${model}"
    time ask via "${QUERY}"
    printf "\\-----------------------\n"
    printf "* After query model=%s nvfree=%s\n" "${model}" "$(nvfree)"        
done
