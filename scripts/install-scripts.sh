#!/bin/bash

SCRIPT_DIR=$(dirname $(realpath "${BASH_SOURCE}"))

DEST_DIR="${1}"
if [ -z "${DEST_DIR}" ];
then
    echo "usage: $0: dest-dir"
    exit 1
fi

FILES="ask.sh bashblock.sh codeblock.sh help-commit.sh nvfree.sh summarize.sh scuttle.sh systype.sh via-api.sh"
SH_FILES="help.sh"

function do_ln {
    local src="$1"
    local dst="$2"
    echo ln -s "${src}" "${dst}"
    ln -s "${src}" "${dst}"
}
    

for file in ${FILES}
do
    do_ln "${SCRIPT_DIR}/${file}" "${DEST_DIR}/${file%.sh}"
done

for file in ${SH_FILES}
do
    do_ln "${SCRIPT_DIR}/${file}" "${DEST_DIR}/${file}"
done
