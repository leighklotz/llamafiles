#!/bin/bash

## Admin client for OpenAPI-compatible LLM server
## Tested with Oobabooga Text Generation Webui
SCRIPT_DIR="$(dirname "$(realpath "${BASH_SOURCE}")")"

# Get site variables from env.sh, if present
[ -z "${IN_LLM_SH_ENV}" ] && [ -f "${SCRIPT_DIR}/env.sh" ] && source "${SCRIPT_DIR}/env.sh"

function usage {
    echo "usage: $0 [--via] [api|cli|--api|--cli] [--get-model-name] [--list-models] [--load-model model-name] [--unload-model] [--list-model-types] [--get-via] [--help]" >> /dev/stderr
    if [ -n "$1" ];
    then
       echo "       $1" >> /dev/stderr
    fi
    exit 1
}

# fixme: better arg handling
function main {
    # first arg or two are optional: [--via] [api|cli|--api|--cli]
    while true;
    do
	case "$1" in
	    --via) shift; ;;
	    --api|--cli|api|cli) VIA="${1#--}"; shift ;;
	    *) break ;;
	esac
    done

    # Load the functions for specified $VIA
    VIA_FUNCTIONS_PATH="$(realpath "${SCRIPT_DIR}/../via/functions.sh")"
    source "${VIA_FUNCTIONS_PATH}"
    #VIA_FUNCTIONS_VIA_X_PATH="$(realpath "${SCRIPT_DIR}/../via/${VIA}/functions.sh")"
    #source_functions "${VIA_FUNCTIONS_VIA_X_PATH}"

    # parse the command args
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
	    --unload-model)
		init_model
		unload_model
		break ;;
	    --list-models)
		init_model
		list_models
		break ;;
	    --list-model-types)
		init_model
		list_model_types
		break ;;
	    --get-via)
		printf "%s\n" "${VIA}"
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
