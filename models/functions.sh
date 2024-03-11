#!/bin/bash

# Check if the script is being sourced or directly executed
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    echo "This script '${BASH_SOURCE[0]}' is intended to be sourced, not executed directly."
    exit 1
fi

# Find the first existing executable or GGUF in the list.
# file_path=$(find_first_model /path/to/file1 /path/to/file2 /path/to/file3)
function find_first_model() {
  local files=("$@")
  local file
  for file in "${files[@]}"; do
    [ $VERBOSE ] && echo "* Checking Model $file" >> /dev/stderr
    if [ -x "$file" ] || [ "${file##*.}" == "gguf" ];
    then
      [ $VERBOSE ] && echo "* Accepting Model $file" >> /dev/stderr
      echo "${file}"
      return 0
    fi
  done
  echo "* Cannot find executable model in $@" >> /dev/stderr
  return 1
}

# Prompt Markup
function alpaca_prompt {

    if [ "${INPUT}" == "" ]; then
	printf -v PROMPT "%s" "Below is an instruction that describes a task. Write a response that appropriately completes the request.

### Instruction:
${SYSTEM_MESSAGE%$'\n'}

${QUESTION%$'\n'}

### Response:

"
    else
	printf -v PROMPT "Below is an instruction that describes a task, paired with an input that provides further context. Write a response that appropriately completes the request.

### Instruction:
%s

%s

### Input:
%s

### Response:

" "${SYSTEM_MESSAGE%$'\n'}" "${QUESTION%$'\n'}" "${INPUT%$'\n'}"
	##### END NO INPUT CASE
    fi
}

function llama_prompt {
    if [ "${INPUT}" == "" ]; then
	printf -v PROMPT "<s>[INST]%s
%s

[/INST]
" "${SYSTEM_MESSAGE%$'\n'}" "${QUESTION%$'\n'}"
    else
	printf -v PROMPT "<s>[INST]%s

%s

%s
[/INST]
" "${SYSTEM_MESSAGE%$'\n'}" "${QUESTION%$'\n'}" "${INPUT%$'\n'}"
    fi
}

function alpaca_prompt {

    if [ "${INPUT}" == "" ]; then
	printf -v PROMPT "%s" "Below is an instruction that describes a task. Write a response that appropriately completes the request.

### Instruction:
${SYSTEM_MESSAGE%$'\n'}

${QUESTION%$'\n'}

### Response:

"
    else
	printf -v PROMPT "Below is an instruction that describes a task, paired with an input that provides further context. Write a response that appropriately completes the request.

### Instruction:
%s

%s

### Input:
%s

### Response:

" "${SYSTEM_MESSAGE%$'\n'}" "${QUESTION%$'\n'}" "${INPUT%$'\n'}"
	##### END NO INPUT CASE
    fi
}

function chatml_prompt {
    if [ "${INPUT}" == "" ]; then
        printf -v PROMPT "<|im_start|>system
%s<|im_end|>
<|im_start|>user
%s<|im_end|>
<|im_start|>assistant" "${SYSTEM_MESSAGE%$'\n'}" "${QUESTION%$'\n'}"
    else
        printf -v PROMPT "<|im_start|>system
%s<|im_end|>
<|im_start|>user
%s
%s<|im_end|>
<|im_start|>assistant" "${SYSTEM_MESSAGE%$'\n'}" "${QUESTION%$'\n'}" "${INPUT%$'\n'}"
    fi
}

