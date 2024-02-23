#!/bin/bash -e

printf '```%s\n' "${1}"
shift
${*}
s=$?
printf '```\n'
exit $?
