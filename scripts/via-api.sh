#!/bin/bash

## Admin client for OpenAPI-compatible LLM server
## Tested with Oobabooga Text Generation Webui
SCRIPT_DIR=$(dirname $(realpath "${BASH_SOURCE}"))

# Get site variables from env.sh, if present
[ -f "${SCRIPT_DIR}/env.sh" ] && source "${SCRIPT_DIR}/env.sh"

MODEL_TYPE="${MODEL_TYPE:-via-api}"
if [ "${MODEL_TYPE}" == "via-api" ];
then
    VIA='api'
else
    VIA='cli'
fi

MODELS_DIRECTORY="$(realpath "${SCRIPT_DIR}/../models")"
MODEL_FUNCTIONS_PATH="$(realpath "${MODELS_DIRECTORY}/${MODEL_TYPE}/functions.sh")"
VIA_FUNCTIONS_PATH="$(realpath "${SCRIPT_DIR}/../via/functions.sh")"
VIA_FUNCTIONS_VIA_X_PATH="$(realpath "${SCRIPT_DIR}/../via/${VIA}/functions.sh")"

source "${VIA_FUNCTIONS_PATH}"
source_functions "${VIA_FUNCTIONS_VIA_X_PATH}"
source_functions "${MODEL_FUNCTIONS_PATH}"


function usage {
    echo "usage: $0 [--get-model-name] [--list-models] [--load-model model-name] [--unload-model] [--help]"
    if [ -n "$1" ];
    then
       echo "       $1"
    fi
    exit 1
}


# if [ "${MODEL_TYPE}" != 'via-api' ];
# then
#    echo "* WARN: MODEL_TYPE=$MODEL_TYPE is not 'via-api', forcing"
#    MODEL_TYPE="via-api"
#fi

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
	--help)
	    usage
	    ;;
	'')
	    usage "at least one flag required"
	    ;;
	*)
	    usage "unrecognized flag $flag"
	    ;;
    esac
}

main "$@"
