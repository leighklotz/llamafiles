# llm-emacs-helper.sh(1) - Interface for LLM interaction from Emacs

## NAME
llm-emacs-helper.sh - Interface for LLM interaction from Emacs

## SYNOPSIS
```
llm-emacs-helper.sh use-case [options] input
```

## DESCRIPTION
`llm-emacs-helper.sh` is a Bash script designed to interface with large language models (LLMs) from within Emacs. It provides a bridge between Emacs and external LLM tools, enabling tasks such as code rewriting, summarization, question answering, and code completion through standardized prompts and processing.

The script accepts input via stdin and outputs results to stdout, making it suitable for integration with Emacs Lisp functions via shell commands.

## USE CASES
The script supports multiple use cases:

- **rewrite** - Re-write code or text according to user instructions
- **ask** - Answer questions about input content  
- **write** - Generate new content based on input and a prompt
- **summarize** - Generate a summary of the input content
- **complete** - Complete code based on partial input and prompt
- **todo** - Process todo items in the input
- **-h** - Display help message

## OPTIONS
- **-n_predict** - Specify the number of tokens to predict. Useful for controlling generation length.
- **--raw-input** - Pass input directly to LLM without processing (useful for specific cases).
- **--help** - Display usage information and exit.

## INPUT
Input is read from stdin, allowing for piped content or direct buffer content from Emacs.

## OUTPUT
Output is written to stdout. For rewrite and todo operations, code lines are prefixed with comments to preserve structure.

## EXAMPLES
```bash
# Ask a question about shell script content
cat my_script.sh | ./llm-emacs-helper.sh ask api mixtral bash "What does this code do?"

# Rewrite code with a prompt
cat my_file.py | ./llm-emacs-helper.sh rewrite cli llama2 python "Improve this code structure"

# Complete code based on a partial input
echo "def hello_world():" | ./llm-emacs-helper.sh complete api phi "finish the function"
```

## ENVIRONMENT
- **DEBUG** - If set, enables debug output showing all arguments and environment variables.
- **LLM_SH** - Path to the LLM execution script (default: `${SCRIPT_DIR}/../scripts/llm.sh`)

## EXIT STATUS
- **0** - Success - LLM completed and output was generated
- **1** - Failure - Invalid arguments, insufficient input, or LLM returned no output
- **2** - Help requested (`-h` option)

## FILES
- `${SCRIPT_DIR}/llm-emacs-helper.sh`
- `${SCRIPT_DIR}/../scripts/llm.sh`

## SEE ALSO
llm.el(1), llm.sh(1)

## AUTHORS
Leigh L. Klotz, Jr.

## COPYRIGHT
Copyright (C) 2024-2025 Leigh L. Klotz, Jr. Licensed under GNU AFFERO GENERAL PUBLIC LICENSE Version 3.

## BUGS
Report bugs to the maintainer.
