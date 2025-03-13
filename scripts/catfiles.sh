#!/bin/bash

SCRIPT_DIR="$(dirname "$(realpath "${BASH_SOURCE[0]}")")"

# todo: parse this from .gitignore and add images
EXCLUDE='(\~|\#.*|\.git.*|__.*|flask_session|\.__pycache__|\.pyc$|\.pyd$|\.pyo$|\.png$|\.jpg$|\.so$|\.o$)'

export FILETYPE="${SCRIPT_DIR}/filetype.sh"
function catblock() {
    local fn="$1"
    local filetype="$(${FILETYPE} $1)"
    printf '#### Content of file %s ####
```%s
%s
```
---

' "${fn}" "${filetype}" "$(cat "${fn}")"
}

printf -v CMD "function %s;\n catblock" "$(declare -f catblock)"

find . -type f | \
    grep -vE "${EXCLUDE}" \
    | xargs -n 1 echo | xargs -I {} bash -c "$CMD {}"
