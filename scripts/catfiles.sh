#!/bin/bash

SCRIPT_DIR="$(dirname "$(realpath "${BASH_SOURCE[0]}")")"

# todo: parse this from .gitignore and add images
EXCLUDE='(\~|\#.*|\.git.*|__.*|flask_session|\.__pycache__|\.pyc$|\.pyd$|\.pyo$|\.png$|\.jpg$|\.so$|\.o$)'
VERBOSE=""
export FILETYPE="${SCRIPT_DIR}/filetype.sh"

function catblock() {
    local fn="$1"
    local filetype="$(${FILETYPE} $1)"
    local file_encoding="$(file --mime-encoding -b $1)"
    if [ "${file_encoding}" == "binary" ] ; then
        echo "Skipping file_encoding=${file_encoding} filename=${fn}: filetype=${filetype}" >> /dev/stderr
    else
        # todo: if file size is greater than 16kb do not cat
        printf '#### Content of file %s ####
```%s
%s
```
---

' "${fn}" "${filetype}" "$(cat "${fn}")"
fi
}

printf -v CMD "function %s;\n catblock" "$(declare -f catblock)"

find . -type f "$@" | \
    grep -vE "${EXCLUDE}" \
    | xargs -n 1 echo | xargs -I {} bash -c "$CMD {}"

exit 0

