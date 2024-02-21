#!/bin/bash -e

printf '```%s\n' "${1}"
shift
${*}
printf '```\n'
