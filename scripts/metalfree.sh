#!/bin/bash

# Check if the system is running on macOS
if [[ "$OSTYPE" == "darwin"* ]]; then
    # Fetch GPU details using system_profiler
    gpu_info=$(system_profiler SPDisplaysDataType | grep -A 5 "Metal Family")

    # Extract relevant information
    if [[ -n "$gpu_info" ]]; then
        echo "$gpu_info"
    else
        echo "No Metal-supported GPU found."
    fi
else
    echo "This script is intended for macOS."
    exit 1
fi
