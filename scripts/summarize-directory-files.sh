#!/bin/bash

SCRIPT_DIR=$(dirname $(realpath "${BASH_SOURCE}"))


if [ -n "${INHIBIT_GRAMMAR}" ];
then
    GRAMMAR_FILE_FLAG=""
else
    GBNF_FILE=$(mktemp -t sumdir-XXXXXX.gbnf)
    GRAMMAR_FILE_FLAG="--grammar-file ${GBNF_FILE}"
cat > ${GBNF_FILE} <<EOF
root ::= "[" [A-Za-z][^\]]+ "]" "(" [^)]+ ")" ": " [^\n]+[\n]
EOF
fi

NLINES_=10
echo "# Files in $(basename $(pwd))"

for FN in *
do
    if [ -f "${FN}" ] ;
    then
	FILETYPE="$(file "${FN}")"
	if [[ "${FILETYPE}" == "* ELF *" ]];
	then
	    NLINES=0
	    echo '**ELF**'
	else
	    NLINES=${NLINES_}
	fi

	PROMPT="For the file named \`${FN}\` and whose filetype is \`${FILETYPE}\` and whose first ${NLINES} lines are shown below, output markdown link in the form \`- [title](${FN}): brief descriptionn\` with \`title\` being a short title for the file, followed by a very brief description of the file contents."
	echo -n "- "
	# set -x
	if [[ "${FILETYPE}" =~ text ]];
	   then
	       ${SCRIPT_DIR}/codeblock.sh '' head -"${NLINES}" "${FN}" | ${SCRIPT_DIR}/help.sh --debug ${GRAMMAR_FILE_FLAG} -e -- "${PROMPT}" || exit 1
	else
	    echo "Not a text file: ${FN}" | ${SCRIPT_DIR}/help.sh ${GRAMMAR_FILE_FLAG} -e -- "${PROMPT}" || exit 1
	fi
    fi
done

rm "${GBNF_FILE}"
