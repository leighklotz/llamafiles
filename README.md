# LLM Help CLI for Linux
- using https://github.com/Mozilla-Ocho/llamafile and scripts from https://github.com/jart

# Script Files
- cli.sh
- llm.sh
- help.sh
- image-name.sh
- rename-pictures.sh [adapted from https://gist.github.com/jart/bd2f603aefe6ac8004e6b709223881c0]
- server.sh
- summarize.sh


## help.sh

USAGE="[-m model-type] [--stdin] [--] QUESTION QUESTION QUESTION"

- `-m`: defaults if not specified; see source for model types
- `--stdin`: reads stdin and interpolates into prompt; if input is a terminal, asks user for input
- `--`: unambiguously terminate args
- `QUESTION`: word or words, can be quoted or not, can be multiline (but also see `--stdin`)

### Bash Coding Example
```bash
$ help.sh -m codebooga -- "split bash argument array into left and right with double hyphen as the separator using special bash builtin functions or operators as needed "
```

### General Linux Example
```bash
$ help.sh what is my ip address
```

### JQ Example

This example takes a live JSON input and shows how to extract a slightly tricky value. As a bonus, the model gives the value of the field.

```bash
$ ./llama.cpp/gguf-py/scripts/gguf-dump.py ./models/dolphin-2.7-mixtral-8x7b.Q4_K_M.gguf --no-tensors --json | help.sh -m dolphin --stdin -- 'give me a jq cli to get the value of the named `llama.context_length` (note the dot is part of the field name) in the following JSON:'

To get the value of `llama.context_length` using jq, you can use the following command:

	```bash
	cat file.json | jq '.metadata["llama.context_length"].value'
	```

Replace `file.json` with the path to your JSON file. This command will output the value of `llama.context_length`, which is 32768 in this case.
$  
```

Proof it works:
```bash
$ ./llama.cpp/gguf-py/scripts/gguf-dump.py ./models/dolphin-2.7-mixtral-8x7b.Q4_K_M.gguf --no-tensors --json | jq '.metadata["llama.context_length"].value'
    32768
$
```

# LLamafile binaries
https://github.com/Mozilla-Ocho/llamafile
- `llamafile-0.1.zip`
- `llamafile-llava-cli-0.1`
- `llamafile-llava-quantize-0.1`
- `llamafile-main-0.1`
- `llamafile-quantize-0.1`
- `llamafile-server-0.1`

# Models and all-in-one
You need to download these from various places on HF and llamafile
- `dolphin-2.5-mixtral-8x7b.Q4_K_M.llamafile`
- `llava-v1.5-7b-q4-main.llamafile`
- `llava-v1.5-7b-q4-server.llamafile`
- `mistral-7b-instruct-v0.1-Q4_K_M-main.llamafile`
