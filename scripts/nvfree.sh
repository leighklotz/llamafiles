#!/bin/bash -e

set -o pipefail
if command -v nvidia-smi &> /dev/null;
   then
       nvidia-smi --query-gpu=memory.free --format=csv,nounits,noheader | awk '{printf "%.2f GiB", $1 / 1024}'
       echo ""
else
    echo "0"
    exit 1
fi
