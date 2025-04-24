#!/bin/bash -e

printf '```bash
$ %b\n' "${*}"
${*}
s=$?
printf '```\n'
exit $?
