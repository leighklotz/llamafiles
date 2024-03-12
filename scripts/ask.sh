#!/bin/bash

SCRIPT_DIR=$(dirname $(realpath "${BASH_SOURCE}"))

MODEL_TYPE=$1
shift
exec ${SCRIPT_DIR}/help.sh -m "${MODEL_TYPE}" "$@"
