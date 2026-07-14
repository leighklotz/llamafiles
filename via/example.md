<<< THIIS IS AN EXAMPLE OF THE FORMAT TO USE -- USE ACTUAL FUNCTIONS AND VARIABLES >>

# via Required Functions
These are the functions that via/functions.sh uses and that are implemented in one of the via/*/functions.sh files:

## prepare_???
- Description: Prepares the foo for the LLM by combining the user question and any additional input provided.
- Usage: prepare_foo [INPUT]
- Input: INPUT: The additional input provided by the user.

# VIA Provided Functions for llm.sh
These are the functions that are defined in via/functions.sh and used in scripts/llm.sh:

## via_set_???
- Description: Sets the foo for the LLM based on the specified foo and other options.
- Usage: via_set_foo name size count

# VIA Provided Functions for vias
These are the functions that are defined in via/functions.sh uses and that are used in one of the via/*/functions.sh files:

## via_set_options
- Description: Sets the options for the LLM based on the specified VIA and other options.
- Usage: via_set_options [OPTIONS]
- Input: OPTIONS: The options to be set for the LLM.

# Environment Variables
Here are all the uppercase environment variables used in the llamafiles scripts:

- MODEL_SIZE: Specifies the model size type to be used for the LLM.
