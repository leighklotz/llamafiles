#!/bin/bash

SCRIPT_DIR=$(dirname $(realpath "${BASH_SOURCE}"))

while IFS= read -r q
do
    echo "=== ${q} ==="
    for m in $(cat ${SCRIPT_DIR}/../models/model-types.txt)
    do
	echo -n "${m} "
	time help.sh -m "${m}" "${q}"
	echo ""
    done
echo ""
done < ${SCRIPT_DIR}/questions.txt
