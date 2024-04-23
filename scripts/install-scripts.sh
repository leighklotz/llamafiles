#!/bin/bash

SCRIPT_DIR=$(dirname $(realpath "${BASH_SOURCE}"))

FILES="ask.sh bashblock.sh codeblock.sh help-commit.sh nvfree.sh machelp.sh summarize.sh scuttle.sh systype.sh via-api.sh"
SH_FILES="help.sh"

DEST_DIR="${1}"
if [ -z "${DEST_DIR}" ];
then
    echo "usage: $0: dest-dir"
    exit 1
fi

function lnf {
    local src="$1"
    local dst="$2"
    if [ -z "$src" ] || [ -z "$dst" ] ;
    then
	echo "fail: lnf src=$src dst=$dst"
	exit 1
    fi
    if [ ! -e "${dst}/" ]; then
	echo "* ln -s ${src} ${dst}"
	ln -s "${src}" "${dst}"
    else
	true
	#echo "${dst}" exists
    fi
}

for file in ${FILES}
do
    lnf "${SCRIPT_DIR}/${file}" "${DEST_DIR}/${file%.sh}"
done

for file in ${SH_FILES}
do
    lnf "${SCRIPT_DIR}/${file}" "${DEST_DIR}/${file}"
done
