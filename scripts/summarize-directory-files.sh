#!/bin/bash

SCRIPT_DIR=$(dirname $(realpath "${BASH_SOURCE}"))
GBNF_FILE=$(mktemp -t sumdir-XXXXXX.gbnf)
NLINES_=10

cat > ${GBNF_FILE} <<EOF
root ::= "[" [A-Za-z][^\]]+ "]" "(" [^)]+ ")" ": " [^\n]+[\n]
EOF

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

	PROMPT="For the file named ${FN} whose type is ${FILETYPE} and whose first ${NLINES} lines are shown below, generate a title and output '[title](${FN})' markdown link with link text being a short title of the file, followed by a very brief description of the file contents."
	echo -n "- "
	#set -x
	${SCRIPT_DIR}/codeblock.sh '' head -${NLINES} ${FN} | \
	    ${SCRIPT_DIR}/help.sh ${*} --grammar-file ${GBNF_FILE} \
	      -e -- "${PROMPT}"
    fi
done

rm "${GBNF_FILE}"
