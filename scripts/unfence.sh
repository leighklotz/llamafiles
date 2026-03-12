#!/bin/bash -e

# extract content, usually code, between triple-backquotes
# opposite of codefence.sh, bashblock.sh etc.
# If you use `lx` you will have extra `---` in your output; use `unlx` unstead of unfence

awk '/^```.*$/ { flag = 1; next } /^```$/ { flag = 0; next } flag { print }'
