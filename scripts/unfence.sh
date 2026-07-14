#!/bin/bash -e

# extract content, usually code, between triple-backquotes
# opposite of codefence.sh, bx.sh etc.
# If you use `lx` you will have extra `---` in your output; use `unlx` unstead of unfence

awk '
  /^```/          { flag = !flag; next }
  flag             { print }           # inside a fence
'

