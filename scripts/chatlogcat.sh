#!/bin/bash

# Script to parse logcat JSON and format user/assistant turns.

# todo:
# if filename is of the form `.../Assistant/20250422-15-09-51.json`
# then use 'Assistant' as the Asssistant name.
# If it is `.../AI/20250422-15-09-51.json`
# then use 'AI' as the Asssistant name.
# etc.


# Usage function
usage() {
  echo "Usage: $0 <log_file> [-i]"
  echo "  <log_file>: Path to the logcat JSON file."
  echo "  -i: Use '.internal' instead of '.visible' in the JQ selector."
  exit 1
}

# Initialize variables
FN=""
INTERNAL=false
ASSISTANT_NAME="Assistant"

# Parse command line arguments
while getopts "i" opt; do
  case $opt in
    i)
      INTERNAL=true
      ;;
    \?)
      echo "Invalid option: -$OPTARG" >&2
      usage
      ;;
    :)
      echo "Option -$OPTARG requires an argument." >&2
      usage
      ;;
  esac
done

shift $((OPTIND - 1)) # Shift past the parsed options

# Check for filename argument
if [ -z "$1" ]; then
  echo "Error: Log file not specified." >&2
  usage
fi

FN="$1"

# Check if the file exists
if [ ! -f "$FN" ]; then
  echo "Error: File '$FN' not found." >&2
  exit 1
fi

# Extract assistant name from dirname
ASSISTANT_NAME=$(basename $(dirname "$FN"))

# Determine JQ selector based on --internal flag
if $INTERNAL; then
  JQ_SELECTOR=".internal[]"
else
  JQ_SELECTOR=".visible[]"
fi

# Process the log file with JQ
cat "$FN" | jq -r "$JQ_SELECTOR | \"\n\n**User:** \(. [0])\n\n**$ASSISTANT_NAME:** \(. [1])\n\n\""
