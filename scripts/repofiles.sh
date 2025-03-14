#!/bin/bash -e

SCRIPT_DIR="$(dirname "$(realpath "${BASH_SOURCE[0]}")")"

VERBOSE=""
export FILETYPE="${SCRIPT_DIR}/filetype.sh"

function catblock() {
    local fn="$1"
    local filetype="$(${FILETYPE} $1)"
    local file_encoding="$(file --mime-encoding -b $1)"
    if [ "${file_encoding}" == "binary" ] ; then
        echo "* Skipping file_encoding=${file_encoding} filename=${fn}: filetype=${filetype}" >> /dev/stderr
    else
        # todo: if file size is greater than 16kb do not cat
        printf '#### Content of file %s ####\n' "${fn}"
        printf '```%s\n' "${filetype}"
        cat "${fn}"
        printf '\n\n```\n'
        printf '#### End of file %s ####\n' "${fn}"
    fi
}

printf -v CMD "function %s;\n catblock" "$(declare -f catblock)"

for fn in $(git ls-files); do
    catblock "$fn"
done

# find sometimes gives weird exit codes
<<<<<<< HEAD:scripts/repofiles.sh
exit 0

=======
#exit 0
>>>>>>> 0993a52 (catfiles: now git based so remove EXCLUDE):scripts/catfiles.sh
