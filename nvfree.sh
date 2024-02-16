#!/bin/bash

nvidia-smi --query-gpu=memory.free --format=csv,nounits,noheader | awk '{printf "%.2f GiB", $1 / 1024}'
echo ""
