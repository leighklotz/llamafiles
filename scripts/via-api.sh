#!/bin/bash

## Nonce script for querying text-generation-webui and llamafile and llama.cpp HTTP API
SCRIPT_DIR=$(dirname $(realpath "${BASH_SOURCE}"))
MODEL_TYPE="${MODEL_TYPE:-via-api}"

function usage {
    echo "usage: $0 [--get-model-name] [--list-models] [--load-model model-name] [--unload-model]"
    echo "       $1"
    exit 1
}


if [ "${MODEL_TYPE}" != 'via-api' ];
then
   usage "\$MODEL_TYPE=$MODEL_TYPE is not 'via-api'"
fi

# fixme: dup code from llm.sh
function setup {
    MODELS_DIRECTORY="$(realpath "${SCRIPT_DIR}/../models")"
    MODEL_FUNCTIONS_PATH="$(realpath "${MODELS_DIRECTORY}/${MODEL_TYPE}/functions.sh")"
    FUNCTIONS_PATH="$(realpath "${MODELS_DIRECTORY}/functions.sh")"

    # Check if the functions file exists
    if [[ -f "${FUNCTIONS_PATH}" ]]; then
	source "${FUNCTIONS_PATH}"
    else
	echo "* ERROR: Cannot find functions: ${FUNCTIONS_PATH}"
	exit 3
    fi

    # Check if the model functions file exists
    if [[ -f "${MODEL_FUNCTIONS_PATH}" ]]; then
	source "${MODEL_FUNCTIONS_PATH}"
    else
	echo "* ERROR: Cannot find model functions for ${MODEL_TYPE}: ${MODEL_FUNCTIONS_PATH}"
	exit 1
    fi
}

# fixme: better arg handling
function main {
    local flag="$1"; shift
    case "$flag" in
	--get-model-name)
	    get_model_name
	    ;;
	--load-model)
	    load_model "$1"
	    ;;
	--list-models)
	    list_models
	    ;;
	--unload-model)
	    unload_model
	    ;;
	'')
	    usage "at least one flag required"
	    ;;
	*)
	    usage "unrecognized flag $flag"
	    ;;
    esac
}

setup
main "$@"
