#!/bin/bash -e

echo '```'"$1"
shift
${*}
echo '```'
