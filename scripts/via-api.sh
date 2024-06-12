#!/bin/bash

SCRIPT_DIR=$(dirname $(realpath "${BASH_SOURCE}"))
exec ${SCRIPT_DIR}/via.sh ${*}
