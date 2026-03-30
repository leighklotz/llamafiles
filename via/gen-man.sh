#!/bin/bash

via --get-model-name

cd ~/wip/llamafiles/via/..

man_prompt='The first 3 files below are part of the SPI of an LLM system, followed by a markdown boilerplate example.md. Read the bash files and note the
functions and variables defined and used. Use that information to write a README.md file for via/function.sh. In the README, separately describe the bash
functions and environment variables that constitute the SPI downward to the via/api implementations, an the API provided to the llm.sh caller.
Document every function and uppercase variable used. When generating README, please format the output using the example.md boilerplate as an example.'

man_prompt="These are the functions that are defined in via/functions.sh and used in scripts/llm.sh:"

(bashblock cat via/functions.sh; bashblock cat via/api/functions.sh; codefence via/example.md) | \
    ask any \ "${man_prompt}" \


