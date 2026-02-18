#!/bin/bash

SCRIPT_DIR="$(dirname "$(realpath "${BASH_SOURCE}")")"

${SCRIPT_DIR}/../models/ggml-sfr-embedding-mistral-q8_0.gguf --server

