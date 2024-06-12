#!/bin/bash

## Admin client for OpenAPI-compatible LLM server
## Tested with Oobabooga Text Generation Webui
SCRIPT_DIR=$(dirname $(realpath "${BASH_SOURCE}"))

# Get site variables from env.sh, if present
[ -f "${SCRIPT_DIR}/env.sh" ] && source "${SCRIPT_DIR}/env.sh"

VIA="${VIA:-api}"

# if [ "${MODEL_TYPE}" != 'via-api' ];
# then
#    echo "* WARN: MODEL_TYPE=$MODEL_TYPE is not 'via-api', forcing"
#    MODEL_TYPE="via-api"
#fi


function usage {
    echo "usage: $0 [--via] [api|cli] [--get-model-name] [--list-models] [--load-model model-name] [--unload-model] [--help]"
    if [ -n "$1" ];
    then
       echo "       $1"
    fi
    exit 1
}

# fixme: better arg handling
function main {
    # first arg or two are optional: [--via] [api|cli]
    while true;
    do
	case "$1" in
	    --via) shift; ;;
	    api|cli) VIA=$1; shift ;;
	    *) break ;;
	esac
    done

    MODELS_DIRECTORY="$(realpath "${SCRIPT_DIR}/../models")"
    VIA_FUNCTIONS_PATH="$(realpath "${SCRIPT_DIR}/../via/functions.sh")"
    VIA_FUNCTIONS_VIA_X_PATH="$(realpath "${SCRIPT_DIR}/../via/${VIA}/functions.sh")"

    source "${VIA_FUNCTIONS_PATH}"
    source_functions "${VIA_FUNCTIONS_VIA_X_PATH}"

    while true;
    do
	local flag="$1"; shift
	case "$flag" in
	    -m|--model_type)
		export MODEL_TYPE="$1"
		shift ;;
	    --get-model-name)
		init_model
		get_model_name
		break ;;
	    --load-model)
		init_model
		load_model "$1"
		shift
		break ;;
	    --list-models)
		init_model
		list_models
		break ;;
	    --unload-model)
		init_model
		unload_model
		break ;;
	    --help)
		usage ;;
	    '')
		usage "at least one flag required" ;;
	    *)
		usage "unrecognized flag $flag" ;;
	esac
    done
}

main "$@"
