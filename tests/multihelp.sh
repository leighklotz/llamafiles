#!/bin/bash

SCRIPT_DIR="$(dirname "$(realpath "${BASH_SOURCE}")")"
VIA_DIRECTORY="$(realpath "${SCRIPT_DIR}/../via")"

source "${VIA_DIRECTORY}/logging.sh"

INFO=1

export USE_SYSTEM_MESSAGE
export INFERENCE_MODE
for USE_SYSTEM_MESSAGE in "" "1"
do
    for INFERENCE_MODE in chat instruct chat-instruct
    do
        log_info "sysmsg=$USE_SYSTEM_MESSAGE $INFERENCE_MODE"
        help.sh "$*"
        echo ""
    done
done
