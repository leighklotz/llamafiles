#!/bin/bash

THRESHOLD=0.4

emgrep --threshold $THRESHOLD ${*} | while IFS= read -r line
do
   IFS=' ' read -r score file <<< "$line"
   echo "Summarizing ${file} ($score)"
   echo "$(cat "$file" | head -200 | ask briefly summarize topic chat of this JSON chat log)"
   echo "---"
   echo ""
done

# $ ~/wip/llamafiles/scripts/chat-summaries.sh Logo
# Summarizing 20240117-16-00-43.json (0.5240)
# The chat log discusses a user asking for a list of Logo primitives in the given code, which is a version of the Logo Language Interpreter for the Apple-II-Plus personal computer. The Logo primitives listed are:

