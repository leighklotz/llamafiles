#!/bin/bash

# Check if two files are provided
if [ "$#" -ne 2 ]; then
    echo "Usage: $0 <fileA> <fileB>"
    exit 1
fi

fileA="$1"
fileB="$2"

diff -u "$fileA" "$fileB" | awk '
  /^--- / { next } 
  /^+++ / { next }
  /^@@ / { next }
  /^-/ { print "<<<<<<< " FILENAME_A; print substr($0,2); next }
  /^\+/ { print "======="; print substr($0,2); print ">>>>>>> " FILENAME_B; next }
  { print }
' FILENAME_A="$fileA" FILENAME_B="$fileB"
