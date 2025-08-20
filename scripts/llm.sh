#!/bin/bash

### **TODO – Remove all CLI‑only plumbing from `llm.sh` while keeping the API path intact**
###  Strip the script down to a pure API driver, keeping every comment, variable, and helper that the API path needs, and removed every CLI‑only plumbing. The new `llm.sh` now only accepts `--via api` (and the legacy `--via cli` still triggers a helpful error), parses the API‑relevant flags, and forwards the request to `via/api/functions.sh`. All local‑runner logic, model‑path handling, GPU/NG‑L configuration, and context‑length math must be deleted or commented out, so the script is lean, testable, and future‑proof.

### 
### > **Goal**:  
### > Convert `llm.sh` to an *API‑only* driver.  
### > All options, functions, and environment variables that are only required for a local llamafile/CLI runner should be deleted.  
### > The script must still accept the same overall interface (e.g., `--via`, `--grammar-file`, `--info`, `--verbose`, etc.) and dispatch to the existing `via/api/functions.sh` helper.
### 
### > **Steps**  
### > 1. **Keep**:  
### >    - `--via` flag (must still accept `--via api` or `--via cli`).  
### >    - All options that are needed by the API: `--model-type` (used only as the *model name* passed to the server), `--temperature`, `--n-predict`, `--grammar-file`, `--info`, `--verbose`, `--debug`, `--log-disable`, `--system-message`, `--use-system-role`.  
### >    - The logic that reads a question from the command line or from `stdin`.  
### >    - The call to `via_set_options` (API version) and `via_api_perform_inference`.  
### 
### > 2. **Remove** (or comment out) the following **CLI‑only** constructs:  
### 
### | Item | Reason | Action |
### |------|--------|--------|
### | `--model-type` / `-m` flag (sets `MODEL_TYPE`) | In API mode the server selects the model; local path is irrelevant. | Delete parsing block; keep as a positional argument if you want to override the server default. |
### | `--context-length` / `-c` flag | Context length is fixed at model load time on the server. | Delete flag and any related variable (`CONTEXT_LENGTH`). |
### | `--ngl` flag | GPU memory limit logic is not needed for API. | Delete flag, variable, and any usage. |
### | `--fast` / `--long` flags | Priority handling is a CLI‑runner feature only. | Delete parsing block and `PRIORITY` variable. |
### | `--batch-size` | Not exposed by the API. | Delete variable and usage in `cli_perform_inference`. |
### | `--seed` | Server decides seed; local flag is meaningless. | Delete variable. |
### | `--gpu` | GPU selection happens on the server. | Delete variable and any `gpu_check`/`set_model_runner` logic. |
### | `--max_tokens` (used in CLI) | API passes `max_new_tokens` in JSON; keep the API param but **not** the CLI local flag. | Remove local flag handling; keep `N_PREDICT` as the API `max_new_tokens`. |
### | `--noerror` | Unused in API path; error output is already controlled by `ERROR_OUTPUT`. | Delete flag and variable. |
### | `--raw-input` logic | API already accepts raw stdin; keep the flag but delete any local path logic that modifies `PROMPT`. | Keep the flag, but remove the code that reads `PROMPT` from a file. |
### | `--process-question-escapes` | This is generic and can stay. | Keep. |
### | `--info` | Already part of API call; keep. | Keep. |
### | `--verbose/-v` | Generic verbosity; keep. | Keep. |
### | `--debug` | Generic debugging; keep. | Keep. |
### | `--log-disable` | Generic logging control; keep. | Keep. |
### | `--system-message` | Used to build JSON; keep. | Keep. |
### | `--use-system-role` | Only relevant for API; keep. | Keep. |
### | `--grammar-file` handling in `cli_perform_inference` | API passes grammar file directly; delete CLI side. | Delete local path handling, keep the variable. |
### 
### > 3. **Remove all function definitions that only exist for CLI**:  
### >    - `cli_set_model_path`  
### >    - `cli_perform_inference`  
### >    - `cli_set_model_path` (duplicate)  
### >    - `set_model_runner`, `gpu_check`, `cap_ngl`, `set_threads` (only for local execution).  
### >    - `truncate_to_context_length` (context length logic).  
### 
### > 4. **Delete any references to `via/cli/functions.sh`**:  
### >    - In the header, the line `VIA_CLI_FUNCTIONS_PATH="$(realpath …)"` and the corresponding `source` line.  
### >    - The guard `if [ -n "$VIA_CLI_FUNCTIONS_LOADED" ]; then …` that triggers the error message.  
### 
### > 5. **Adjust `via_set_options`**:  
### >    - Keep the API version that sets `repeat_penalty` and `penalize_nl`.  
### >    - Remove the CLI version that builds command‑line flags.  
### 
### > 6. **Update the main dispatch**:  
### >    ```bash
### >    case "${VIA}" in
### >      "api")   via_set_options; via_api_perform_inference … ;;
### >      "cli")   log_and_exit 1 "CLI mode removed – use --via api only."
### >    esac
### >    ```  
### >    This ensures any accidental `--via cli` usage fails cleanly.  
### 
### > 7. **Re‑order and tidy**:  
### >    - Move the default section to only set API‑relevant variables (`MODEL_TYPE`, `TEMPERATURE`, `N_PREDICT`, `GRAMMAR_FILE`, `INFO`, `VERBOSE`).  
### >    - Remove any `DEFAULT` or `ENV` mapping that refers to CLI variables.  
### 
### > 8. **Run tests**:  
### >    - Execute `llm.sh "What is the capital of France?"` – should hit the API and print the answer.  
### >    - `llm.sh --via api -m dolphin-2.7-mixtral "Explain polymorphism in Python."` – should work.  
### >    - Verify that all removed options no longer appear in `--help` output.  
### 
### > **Deliverable**: After applying the above changes, `llm.sh` should be a lean, API‑only driver that still supports the original `--via`, `--grammar-file`, `--info`, `--verbose`, etc., but no longer contains any code that depends on the local llamafile runner.
### 
SCRIPT_DIR="$(dirname "$(realpath "${BASH_SOURCE}")")"

USAGE='[--via api] | [ --via cli [-m|--model-type model-type] [--fast | --long] [--context-length|-c n] [--ngl n] [--n-predict n] ] [--stdin|--interactive|-i] [--temperature temp] [--n-predict n] [--grammar-file file.gbnf] [--info] [--verbose|-v] [--debug] [--] QUESTION*

Mode of operation:
- --via api: Specifies API mode.
- --via cli: Specifies CLI mode.

General options:
- --model-type or -m: Sets the model type (e.g., mistral).
- --n-predict: Sets the number of prediction tokens.
- --temperature: Sets the temperature for sampling.
- --grammar-file: Provides a grammar file path (file.gbnf).
- --info: Displays info logging level.
- --verbose or -v: Enables verbose logging.
- --debug: Enables debug logging.
- --noerror: Suppresses error output.
- --: Separates command-line arguments from input question prompt.

Input options:
- --stdin, --interactive, or -i: Force reading input from stdin, even if not in a pipe.
- --raw-input: Uses stdin as the only text to send to the model.
- --process-question-escapes or -e: Processes escape sequences in the input question.

Priority and context options (apply to --via cli only):
- --fast: Sets priority for generation speed.
- --long: Sets priority for generation length.
- --context-length or -c: Sets the context length.
- --ngl: Sets the NGL parameter.

Help option:
- --help: Displays help information and usage.

llm.sh is intended for use by other, friendlier scripts, such as help.sh and ask, both of which have their own options
and also accept llm.sh options as listed above.

To use llm.sh directly, you can pass these options to the script name followed by any input questions. For example:

$ llm.sh --via cli --model-type mistral --temperature 0.7 "What is the capital of France?"

Environment variables are used to configure the behavior of llm.sh. Here are the available environment variables:

- LLM_SH_ENV: A flag to indicate that llm.sh environment is being loaded. Used internally.
- IN_LLM_SH_ENV: A flag to indicate that the environment is already loaded. Used internally.
- MODEL_TYPE: The type of model to use (e.g., mistral, vicuna, etc.). Default: mistral.
- INFERENCE_MODE: The mode of inference (chat, instruct, chat-instruct): Default: chat-instruct
- VIA: The mode of operation. Can be ''api'' or ''cli''. Default: cli.
- GPU: The GPU configuration. Can be ''auto'', ''nvidia'', or ''omit''. Default: auto.
- GRAMMAR_FILE: The path to a grammar file (file.gbnf).
- BATCH_SIZE: The batch size for processing.
- SEED: The random seed for model generation. Default: -1 (automatic).
- LLAMAFILE_MODEL_RUNNER: The path to the llamafile model runner script. Default: $(realpath "${SCRIPT_DIR}/../lib/llamafile-0.6.2").
- FORCE_MODEL_RUNNER: Force the use of a specific model runner script.
- NGL: The NGL parameter. Applies only to ''--via cli'' mode.
- CONTEXT_LENGTH: The context length. Applies only to ''--via cli'' mode.
- PRIORITY: The priority for generation. Can be ''speed'', ''length'', or ''manual''. Applies only to ''--via cli'' mode. Default: manual.
- TEMPERATURE: The temperature for sampling. Can be overridden by the --temperature command-line option.
- N_PREDICT: The number of prediction tokens to generate. Can be overridden by the --n-predict command-line option.
- SYSTEM_MESSAGE: The system message to use. Can be overridden by the --system-message command-line option.
- RAW_FLAG: A flag to indicate that stdin should be used as the only text to send to the model. Default: --raw-input if --raw-input is specified.
- ERROR_OUTPUT: The file descriptor for error output. Default: /dev/null (suppresses error output).
- KEEP_PROMPT_TEMP_FILE: Determines which prompt temp files to keep. Can be ''NONE'', ''ERROR'', or ''ALL''. Default: ALL.
- LOG_DISABLE: A flag to disable logging. Can be overridden by the --log-disable command-line option.
- VERBOSE, INFO, DEBUG: Logging levels. Can be overridden by the --verbose, --info, and --debug command-line options, respectively.
- Silent variables (SILENT_PROMPT, NO_PENALIZE_NL, REPEAT_PENALTY): These variables are not settable by environment variables.
'


function usage {
    printf "Usage: %s %s\n" "$0" "${USAGE}" >> /dev/stderr
}

### If there are any args, require "--" or any non-hyphen word to terminate args and start question.
### Assume the whole args is a question if there is no hyphen to start.
### There may be no question, if all is contained in SYSTEM_MESSAGE and STDIN.
INPUT=""
: "${QUESTION:=}"
DO_STDIN="$(test -t 0 || echo $?)"

function parse_args() {
    if [[ "${1}" == "-"* ]];
    then
        while [[ $# -gt 0 ]]; do
            case $1 in
                --help)
		    usage
                    exit 0
                    ;;
                --via) shift; VIA="$1" ;;
                --model-type|-m) shift; MODEL_TYPE="$1" ;;
		--inference-mode) shift; INFERENCE_MODE="$1" ;;
		# Logging 
                --info) INFO=1 ;;
                --verbose|-v) VERBOSE=1 ;;
                --debug) # LOG_DISABLE=" " to override default
		    ERROR_OUTPUT="/dev/stdout"; SILENT_PROMPT=""; DEBUG=1; LOG_DISABLE=" "
		    ;;
                --noerror) ERROR_OUTPUT="/dev/null" ;;
		# Generation options
                --n-predict) shift; N_PREDICT="$1" ;;
                --temperature) shift; TEMPERATURE="$1" ;;
                --grammar-file) shift; GRAMMAR_FILE="$1" ;;
		# Input Options
		--stdin|--interactive|-i) DO_STDIN=1 ;;
                --raw-input) RAW_FLAG="--raw-input" ;;
                --process-question-escapes|-e) PROCESS_QUESTION_ESCAPES=1 ;;
		# todo: these next four options only apply to --cli models so move them to via/cli
                --fast) PRIORITY="speed" ;;
                --long) PRIORITY="length" ;;
                --context-length|-c) shift; CONTEXT_LENGTH="$1" ;;
                --ngl) shift; NGL="$1" ;;
		# prompt
                --) shift; QUESTION=("$*"); break ;; # consumes rest of line
                -*) printf "Unrecognized option: %s\n\n" "$1" >> /dev/stderr
		    usage
		    exit 1
		    ;;
                *) QUESTION=("$*"); break ;; # consumes rest of line
            esac
            shift
        done
    else
        QUESTION=("$*")
    fi
}

parse_args ${@}

###
### Site Variables
### Set site variables from env.sh
###
[ -z "${IN_LLM_SH_ENV}" ] && [ -f "${SCRIPT_DIR}/env.sh" ] && source "${SCRIPT_DIR}/env.sh"

###
### Process flags and environment variables
###

: "${VIA:=cli}"
: "${MODEL_TYPE:=mistral}"
: "${INFERENCE_MODE:=chat-instruct}"
# Logging
: "${INFO:=${VERBOSE}}"
: "${VERBOSE:=}"
: "${DEBUG:=}"
: "${LOG_DISABLE:=--log-disable}" # use space ' ' to override
: "${KEEP_PROMPT_TEMP_FILE:=ALL}" # "NONE"|"ERROR"|"ALL"
# Old versions of llamafile sometimes need -silent-prompt or --no-display-prompt
# edit this, or use FORCE_MODEL_RUNNER and a newer MODEL_RUNNER .
: "${SILENT_PROMPT:=--silent-prompt --no-display-prompt}"
# Geneation Options
: "${TEMPERATURE:=}"
: "${N_PREDICT:=}"
: "${SYSTEM_MESSAGE:=}" # used to default to "Answer the following user question:"
: "${GPU:=auto}"        # auto|nvidia|omit
: "${GRAMMAR_FILE:=}"
: "${BATCH_SIZE:=}"
: "${SEED:=-1}"
# Extra
: "${LLM_ADDITIONAL_ARGS:=}"
# todo: move these to CLI module
: "${FORCE_MODEL_RUNNER:=}"
: "${LLAMAFILE_MODEL_RUNNER:="$(realpath "${SCRIPT_DIR}/../lib/llamafile-0.6.2") -m"}"
: "${NGL:=}"
: "${CONTEXT_LENGTH:=}"
: "${PRIORITY:=manual}" # speed|length|manual


###
### These variables are not settable by environment variables
###
PROCESS_QUESTION_ESCAPES=""
MODEL_RUNNER="/usr/bin/env"
RAW_FLAG=""
ERROR_OUTPUT="/dev/null"
NO_PENALIZE_NL=""

###
### Load functions for API or CLI
###
VIA_DIRECTORY="$(realpath "${SCRIPT_DIR}/../via")"
FUNCTIONS_PATH="$(realpath "${VIA_DIRECTORY}/functions.sh")"
VIA_CLI_FUNCTIONS_PATH="$(realpath "${VIA_DIRECTORY}/cli/functions.sh")"
VIA_API_FUNCTIONS_PATH="$(realpath "${VIA_DIRECTORY}/api/functions.sh")"
source "${FUNCTIONS_PATH}"

if [ -n "$DEBUG" ]; then
    trap 'print_stack_trace' INT
fi

###
### Prompt and STDIN processing
# todo: move this to where it is used
PROMPT_TEMP_FILE="$(mktemp_file "prompt")"
register_temp_file "${PROMPT_TEMP_FILE}"

# Process escape sequences in QUESTION if requested.
# This will turn literal "\n" into literal newline in the QUESTION>
# STDIN is never processed for escapes.
function process_question_escapes() {
    if [ -n "${PROCESS_QUESTION_ESCAPES}" ]; then
        log_verbose "Processing escape sequences in QUESTION"
        printf -v QUESTION "%b" "$QUESTION"
    fi
}

# Read STDIN into INPUT, and prompt if stdin is tty
function process_stdin() {
    if [ -n "$DO_STDIN" ];
    then
        if [ -t 0 ];
        then
            echo "Give input followed by Ctrl-D:"
        fi
        INPUT="$(cat | tr '\0' ' ')"
    fi
}

###
### Debug and log
###

function set_verbose_debug {
    # Set verbose and debug last
    if [ -n "${DEBUG}" ] || [ -n "${INFO}" ] || [ -n "${VERBOSE}" ];
    then
        log_info "Parameters: ngl=${NGL} context_length=${CONTEXT_LENGTH} est_len=${PROMPT_LENGTH_EST}"
    fi
    if [ -n "${DEBUG}" ];
    then
        set -x
    fi
}

# Try to inform user about errors
function report_success_or_fail {
    local status="$1"
    if [ $status -ne 0 ];
    then
        if [ "${ERROR_OUTPUT}" == "/dev/null" ];
        then
            log_error "FAIL STATUS=$status: re-run with --debug"
        else
            log_error "FAIL STATUS=$status: errors went to ${ERROR_OUTPUT}" > /dev/stderr
        fi
    fi
    return $status
}

# if --raw-input is specified, use stdin as the only text to send to the model
function adjust_raw_flag {
    if [ -n "${RAW_FLAG}" ]; then
        PROMPT="${INPUT}"
        SYSTEM_MESSAGE=""
    fi
}

###
### Perform Inference
###

function perform_inference() {
    case "${VIA}" in
        "api")
            via_set_options
            via_api_perform_inference "${MODEL_TYPE}" "${INFERENCE_MODE}" "${SYSTEM_MESSAGE}" "${PROMPT}" "${GRAMMAR_FILE}" "${TEMPERATURE}" "${repeat_penalty}" "${penalize_nl}"
            status=$?
            ;;
        "cli")
            via_set_options
            cli_perform_inference
            status=$?
            ;;
        *)
            log_error "Unknown VIA=${VIA}"
            status=1
            ;;
    esac
    return $status
}

###
### Main Flow
###

init_model
process_question_escapes
log_info "process_stdin"
process_stdin
log_info "process_stdin done"
prepare_model
log_info "MODEL_PATH=${MODEL_PATH}"
adjust_raw_flag
log_info "truncate_to_context_length"
truncate_to_context_length
set_verbose_debug
log_info "perform_inference"
perform_inference; STATUS=$?
log_info "perform_inference done"
report_success_or_fail $STATUS
cleanup_temp_files $STATUS
exit $STATUS

