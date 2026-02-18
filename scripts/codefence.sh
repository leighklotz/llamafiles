#!/bin/bash -e

SCRIPT_DIR="$(dirname "$(realpath "${BASH_SOURCE}")")"

fn="$1"

if [ -z "${fn}" ]; then
    echo "usage: $0 filename"
    exit 1
fi

if [ ! -r "${fn}" ]; then
    echo "cannot read ${fn}"
    exit 1
fi

ft="$("$SCRIPT_DIR/filetype.sh" "${fn}")"
printf $'```%s
%s
%s
```
' "${fn}" "${ft}" "$(cat "${fn}")"
