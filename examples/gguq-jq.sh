#!/bin/bash -x

~/wip/llama/llama.cpp/gguf-py/scripts/gguf-dump.py \
    ~/wip/oobabooga/text-generation-webui/models/dolphin-2.7-mixtral-8x7b.Q4_K_M.gguf \
    --no-tensors --json | \
    help "give me a jq cli to get value the value of the named llama.context_length in the following JSON:"
