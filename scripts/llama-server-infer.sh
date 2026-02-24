#!/bin/bash

# https://github.com/ggml-org/llama.cpp/blob/master/tools/server/README.md

#NO_WEBUI="--no-webui"
NO_WEBUI=""
# VERBOSE="--verbose"
VERBOSE=""
HOST="--host 0.0.0.0"
PORT="--port 5000"
LLAMA_SERVER=/home/klotz/wip/oobabooga/text-generation-webui/installer_files/env/lib/python3.11/site-packages/llama_cpp_binaries/bin/llama-server
MODEL_DIR=/home/klotz/wip/models
PRESETS_DIR=/home/klotz/wip/oobabooga/text-generation-webui/user_data/presets
MODEL="--model $MODEL_DIR/gemma-3-27b-it-Q4_K_M.gguf"
MMPROJ="--mmproj $MODEL_DIR/mmproj-google_gemma-3-27b-it-bf16.gguf"
PRESETS="gemma-3.yaml"
CTX_SIZE="--ctx-size 14320"
GPU_LAYERS="--gpu-layers 63"
BATCH_SIZE="--batch-size 2048"
UBATCH_SIZE="--ubatch-size 1024"
ROPE_FREQ_SCALE="--rope-freq-scale 0.125"
ROPE_FREQ_BASE="--rope-freq-base 1000000"
FLASH_ATTN="--flash-attention on"

### Convert newline file contents `name:value` pairs
## to double-dash bash CLI pairs.
function presets() {
    fn="$PRESETS_DIR/$1"
    while IFS= read -r line; do
        if [[ "$line" =~ ^([^:]+):(.*)$ ]]; then
            name="--${BASH_REMATCH[1]//_/-}"
            value="${BASH_REMATCH[2]##*[[:space:]]}"
            name="${name//temperature/temp}"
            echo -n "${name} ${value} "
        fi
    done < "$fn"
}

# GLM-4.7-Flash-UD-Q6_K_XL.gguf
# ./llama-server --model downloaded_models/GLM-4.7-Flash-UD-Q6_K_XL.gguf --port 11433 --host "0.0.0.0" -fa on --ctx-size 48000 --temp 0.7 --top-p 1.0 --min-p 0.01 --jinja -ngl 99 $(presets $PRESETS)

presets="$(presets $PRESETS)"
set -x
$LLAMA_SERVER $MODEL $CTX_SIZE $GPU_LAYERS $BATCH_SIZE $UBATCH_SIZE $HOST $PORT $NOWEBUI $VERBOSE $FLASH_ATTENTION $ROPE_FREQ_SCALE $ROPE_FREQ_BASE $MMPROJ $presets 
