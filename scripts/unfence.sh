#!/bin/bash -e

# extract content, usually code, between triple-backquotes
# opposite of codeblock.sh

awk '
{
  line = $0
  while (match(line, /```/)) {
    if (!in_block) {
      # Opening fence: ignore any text on this line.
      in_block = 1
      line = ""  # Discard any characters after the opening fence.
      break     # Stop processing further fences on this line.
    } else {
      # Closing fence: print any text that appears before the fence.
      code = substr(line, 1, RSTART - 1)
      if (length(code) > 0)
        print code
      in_block = 0
      # Continue processing the rest of the line (in case a new block starts).
      line = substr(line, RSTART + RLENGTH)
    }
  }
  # If still inside a code block, print the remaining text.
  if (in_block && length(line) > 0)
    print line
}
' "$@"
