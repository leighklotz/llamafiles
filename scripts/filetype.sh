#!/bin/bash

# Function to convert file type to desired format
convert_filetype() {
    local file=$1
    local output=$(file --mime-type -b "$file")
    case "$output" in
        text/plain)
            echo "plaintext"
            ;;
        text/x-java)
            echo "java"
            ;;
        text/x-lisp)
            echo "lisp"
            ;;
        text/x-script.python | text/x-python)
            echo "python"
            ;;
        application/json)
            echo "json"
            ;;
        application/xml)
            echo "xml"
            ;;
        text/x-shellscript)
            echo "sh"
            ;;
        # Add more cases as needed
        *)
            echo "$output"
            ;;
    esac
}

convert_filetype "$1"

