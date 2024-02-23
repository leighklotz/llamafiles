#!/bin/bash

SCRIPT_DIR=$(dirname $(realpath "${BASH_SOURCE}"))
GBNF_FILE=$(mktemp -t sumdir-XXXXXX.gbnf)
NLINES=100

cat > ${GBNF_FILE} <<EOF
root ::= "[" [A-Za-z][^\]]+ "]" "(" [^)]+ ")"

EOF

echo "# Files in $(basename $(pwd))"

for FN in *
do
    if [ -f "${FN}" ] ;
    then
	PROMPT="For the file named ${FN} whose first ${NLINES} lines are shown below, generate a title and output '[title](${FN})' markdown link with link text being a short title of the file."
	echo -n "- "
	${SCRIPT_DIR}/codeblock.sh ${FN} head -${NLINES} ${FN} | \
	    ${SCRIPT_DIR}/help.sh ${*} --grammar-file ${GBNF_FILE} \
	      -e -- "${PROMPT}"
    fi
done

rm "${GBNF_FILE}"
