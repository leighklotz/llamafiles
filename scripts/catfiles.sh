#!/bin/bash -e

SCRIPT_DIR="$(dirname "$(realpath "${BASH_SOURCE[0]}")")"

VERBOSE=""
export FILETYPE="${SCRIPT_DIR}/filetype.sh"

# Function to parse .gitignore and generate regex
parse_gitignore() {
    local gitignore_file="$1"
    local effective_gitignore

    # Get the effective .gitignore content
    effective_gitignore=$(git ls-files --others --exclude-standard --directory | grep -v '^$')

    # Convert gitignore patterns to regex
    regex_patterns=""
    for line in $effective_gitignore; do
        # Handle different patterns (e.g., directories, files)
        if [[ $line == */ ]]; then
            regex_patterns="$regex_patterns|${line%/}.*"
        elif [[ $line == * ]]; then
            regex_patterns="$regex_patterns|${line//\./\\.}"
        fi
    done

    # Remove leading pipe and add additional patterns
    regex_patterns="$regex_patterns|\.png$|\.jpg$|\.so$|\.o$"
    printf "%s" "${regex_patterns}"
}

EXCLUDE="^.git.*\|$(parse_gitignore .gitignore)"
echo $EXCLUDE >> /dev/stderr
exit 4

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

