# LLM Help CLI for Linux

Using https://github.com/Mozilla-Ocho/llamafile and https://github.com/jart models and scripts.

This repository provides an LLM Help CLI for Linux and Mac systems to provide help, summarization, coding assistance, system information queries, image renaming, etc. Similar to https://justine.lol/oneliners/ and other resources.

## Brief Usage examples
Below are brief examples to show you what these tools do.

Install links to the scripts you want to use in your home bin directory. In the example below, symlink names have been shortened.

```shell
$ help.sh -- "Split bash argument array into left and right with double hyphen as the separator using special bash builtin functions or operators"
$ help.sh what is my ip
$ help.sh -m dolphin 'Give me a JQ cli to get the value of the field named `llama.context_length`'
$ ask dolphin ...
$ man xxd | help.sh "print file contents with xxd without the address column"
$ help-commit 
$ codeblock jq cat foo.jq | help.sh '"Explain this:'
$ codeblock xslt cat redir/target.xslt | help.sh explain

```

More examples can be found below and in the [examples](https://github.com/leighklotz/llamafiles/tree/main/examples) directory.

## help.sh usage and examples
USAGE="[-m|--model-type model-type] [--stdin|--interactive|-i] [--fast | --long] [--temperature temp] [--context-length|-c n] [--ngl n] [--n-predict n] [--debug] [--verbose] [--] QUESTION*"

Uses question words and stdin, if any, to create the model prompt.

- `-m`, `--model-type`: defaults if not specified. Available options are: `mixtral`, `dolphin`, `mistral`, `codebooga`, `deepseek`, `rocket`, and `phi`.
- `--stdin|--interactive|-i`: if input is a terminal, asks user for input and interpolates stdin into prompt 
- `--`: unambiguously terminate args
- `QUESTION`: word or words, can be quoted or not, can be multiline (but also see `--stdin`)
- [other options]: See source
- $ENV: see env.sh.examples or source of llm.sh

Below are a few examples. More are in [examples](examples).

### help.sh Bash Coding Example
```
$ help.sh -m codebooga -- "split bash argument array into left and right with double hyphen as the separator using special bash builtin functions or operators as needed "
```

### help.sh General Linux Example
```
$ help.sh what is my ip address
```

### help.sh JQ Example
This example takes a live JSON input and shows how to extract a slightly tricky value. As a bonus, the model gives the value of the field.

```
$ ./llama.cpp/gguf-py/scripts/gguf-dump.py ./models/dolphin/dolphin-2.7-mixtral-8x7b.Q4_K_M.gguf --no-tensors --json | \
   help.sh -m dolphin 'give me a jq cli to get the value of the named `llama.context_length` (note the dot is part of the field name) in the following JSON:'

To get the value of `llama.context_length` using jq, you can use the following command:

	```
	cat file.json | jq '.metadata["llama.context_length"].value'
	```

Replace `file.json` with the path to your JSON file. This command will output the value of `llama.context_length`, which is 32768 in this case.
$  
```
Here is proof that the JQ expression given above works:

```
$ ./llama.cpp/gguf-py/scripts/gguf-dump.py ./models/dolphin/dolphin-2.7-mixtral-8x7b.Q4_K_M.gguf --no-tensors --json | jq '.metadata["llama.context_length"].value'
    32768
$
```

### help.sh LSHW Example
The Codebooga llamafile I'm using doesn't yet support --silent-prompt so I elided the re-printed prompt manually.
```
sudo lshw | wc 
    727    2411   31214
$ sudo lshw | help.sh -c 16384 -m codebooga --stdin -- 'Summarize the following lshw output:\n\n'

	This output shows information about the hardware components of a
	computer system, including the motherboard, CPU, memory, storage
	devices, and peripherals. The system is a desktop with an Intel Core
	i9-10900K processor, 64GB RAM, and two NVIDIA GeForce RTX 3090
	graphics cards. It has a Samsung SSD 970 EVO 1TB and a Samsung SSD 990
	PRO 4TB as storage devices, and an ASUS ROG MAXIMUS XII APEX
	motherboard with Intel Comet Lake chipset. The system is running
	Ubuntu Linux with kernel version 6.2.0-39-generic has a wireless
	network interface (Intel Comet Lake PCH CNVi WiFi) and an Ethernet
	interface (Intel Ethernet Controller I225-V). The computer also has
	USB ports, audio devices, and an SMBus controller.  [end of text]
```

### help.sh Raspberry Pi 5 lspci with Rocket model
```
klotz@rpi5:~ $ export MODEL=rocket
klotz@rpi5:~ $ help.sh lspci
LSPCI, or List PCI Devices, is a command used in Linux to display information about all the PCI devices connected to the system's motherboard. It provides details such as device vendor and product IDs, memory sizes, and supported devices. This command can be executed in the terminal of a Linux system with root privileges. For example, to list all PCI devices, you would type `lspci` in the terminal and press Enter. The output will display the information about each device.
klotz@rpi5:~ $ lspci | help.sh --stdin "explain this lspci output"
Sure, I can help you understand the LSPCI (List PCI Devices) output you provided.

LSPCI is a command in Linux that lists all PCI devices in the system. The output you provided is in a format that's easy to read but not very human-friendly. Here's a breakdown of what each line means:
1. `0000:00:00.0 PCI bridge: Broadcom Inc. and subsidiaries Device 2712 (rev 21)`: This is a PCI bridge, which is a device that connects PCI buses together. The device ID '2712' is a vendor ID for Broadcom, and the revision '21' is the chipset revision.
2. `0000:01:00.0 Non-Volatile memory controller: Realtek Semiconductor Co., Ltd. RTS5763DL NVMe SSD Controller (rev 01)`: This is an NVMe (Non-Volatile Memory Express) SSD controller from Realtek. The device ID '01' is the chipset revision.
3. `0001:00:00.0 PCI bridge: Broadcom Inc. and subsidiaries Device 2712 (rev 21)`: This is another PCI bridge, connecting PCI buses. The device ID '2712' is the same as before, indicating it's from the same vendor.
4. `0001:01:00.0 Ethernet controller: Device 1de4:0001`: This is an Ethernet controller from the device ID '1de4:0001'. The device ID '1de4' is a vendor ID for Intel, and the revision '1' is the chipset revision.
So, in summary, your system has a Broadcom PCI bridge (Device 2712) and two Realtek NVMe SSD controllers (one at address 0000:01:00.0 and another at address 0001:00:00.0). It also has an Intel Ethernet controller (Device 1de4:0001).

I hope that helps! Let me know if you have any other questions.
klotz@rpi5:~ $ 
```

More examples are in the [examples](https://github.com/leighklotz/llamafiles/tree/main/examples) directory.

## help-commit.sh usage and examples

This script uses llm.sh to generate commit messages from the current directory.

```bash
$ help-commit [--oneline|--multiline] [--staged] [git diff options] [-- llm.sh options]
```

You can choose to provide `--oneline` or `--multiline` flags to control the format of the commit message. Using `--oneline` (or default) will create a single line commit message, and `--multiline` will create a multi-line one. The script uses `git diff --staged` first, then `git diff`.

For example, to create an oneline commit message for staged changes, use the following command:

```bash
help-commit --staged
```

To create multi-line commit message using the mixtral model with a long context, do this:

```bash
help-commit -- -m mixtral --long
```

# llm_el for Emacs
There are many LLM integrations for Emacs; mine is here: (llm_el/)[llm_el/].

# LLamafile binaries
Get these files from (https://github.com/Mozilla-Ocho/llamafile)[(https://github.com/Mozilla-Ocho/llamafile].
They go in `lib/`:

- `llamafile-0.2.1.zip`
- `llamafile-llava-cli-0.2.1`
- `llamafile-llava-quantize-0.2.1`
- `llamafile-main-0.2.1`
- `llamafile-quantize-0.2.1`
- `llamafile-server-0.2.1`
- `zipalign-0.2.1`

# Models and all-in-one
You need to download these from https://huggingface.co/jartine and other places on HF.
todo: script to do this

## Desktop and GPU models
Each model-type directory has one or more `.gguf` or `.llamafile` models and a `function.sh` file.

These go in `models/*`:
- dolphin: `dolphin-2.5-mixtral-8x7b.Q4_K_M.llamafile`
- llava: `llava-v1.5-7b-q4-main.llamafile`, `llava-v1.5-7b-q4-server.llamafile`
- mistral: `mistral-7b-instruct-v0.2.Q4_K_M.llamafile`

## RPI5 and other small models
These go in `models/*`:
- mistral: `mistral-7b-instruct-v0.2.Q3_K_M.llamafile`
- mixtral: `mixtral_7bx2_moe.Q3_K_M.gguf`
- phi: `phi-2.Q5_K_M.llamafile`, `phi-2.Q6_K.llamafile`
- rocket: `rocket-3b.Q4_K_M.llamafile`

# Scripts and Files
These files are in [scripts/](scripts/). You might want to symlink some to your bin directory.
You can also copy [scripts/env.sh.example](scripts/env.sh.example) to `scripts/env.sh` and edit it to set default values.

## base
- llm.sh - the base script that others call

## user programs
- help.sh - CLI for Linux help - cann't be shortened to 'help' in bash
- ask.sh - like 'help.sh -m $1' - can be shortened to 'ask' in bash
- machelp.sh - CLI for Mac help
- summarize.sh - CLI to summarize a hyperlink
- help-commit.sh - CLI to run `git diff` and produce a commit message
- summarize-directory-files.sh - summarize directory files as markdown

## user utilities
- systype.sh - Pipe to help.sh to provide context for distro-specific questions
- nvfree.sh - check your GPU usage
- codeblock.sh [lang] [cmd] - Pipe to help.sh to wrap output of cmd in a codeblock of type lang.
- bashblock.sh [cmd] - Pipe to help.sh to wrap output of cmd in a bash-like template.

## images
These are less developed.
- image-name.sh - Simple script using LLAVA to geneate image name
- rename-pictures.sh [adapted from https://gist.github.com/jart/bd2f603aefe6ac8004e6b709223881c0 and included here under Apache 2.0 license]
- llava-cli.sh 

## internals
- create-chat-templates.py: wip to statically create models/*/functions.sh prompt processing

# llm.sh Details
The help and summary scripts invoke `llm.sh`, but you can use it yourself directly as well.

## Command Line Flags
`llm.sh` accepts the following command line flags:

- `-m` or `--model-type`: specifies the type of model to use. The available options are `mixtral`, `dolphin`, `mistral`, `codebooga`, `deepseek`, `
rocket`, and `phi`.
- `--fast`: sets the priority to speed (more GGUF GPU layers, less context)
- `--long`: sets the priority to length (max context, fewer GGUF GPU layers)
- `--temperature`: sets the temperature of the model.
- `--verbose`: enables verbose mode.
- `-c` or `--context-length`: sets the context length.
- `--ngl`: sets the number of gradient layers.
- `--n-predict`: sets the number of tokens to predict.
- `--debug`: enables debug mode.
- `--noerror`: disables error output.
- `--stdin` or `--interactive` or `-i`: include stdin as input, after the question prompt
- `-e` or `--process-question-escapes`: expand \n and friends in the question; stdin is always literal
  `--raw-input`: prepare for pure completion, ignoring PROMPT and SYSTEM_PROMPT in favor of stdin
- `--`: terminates the arguments and starts the question.

In CLI prompt, be sure to use apostrophe quotes if your prompt contains backquotes, since double-quotes will allow bash to evaluate the result of the backquoted content.

For example, this is good:
```bash
$ help.sh 'How can I use the `yes` command in bash?'
```
but this is bad:
```bash
$ help.sh "How can I use the `yes` command in bash?"
```

## Environment Variables

In addition to these command line flags, the script also checks for several environment variables to configure its behavior:

- `MODEL_TYPE`: The default model type to use if none is specified via the `-m` or `--model-type` flag.
- `TEMPERATURE`: The default temperature parameter for the model if none is specified via the `--temperature` flag.
- `CONTEXT_LENGTH`: The default context length for the model if none is specified via the `--context-length` or `-c` flag.
- `N_PREDICT`: The default number of tokens to predict if none is specified via the `--n-predict` flag.
- `SYSTEM_MESSAGE`: The default system message to use if none is specified via the command line.
- `MODEL_RUNNER`: The program used to run the model. Defaults to `/usr/bin/env` and automatically set to `llamafile...` if needed. See also `FORCE_MODEL_RUNNER`
- `LLM_ADDITIONAL_ARGS`: Value is interpolated into the call to LLM at the end of the CLI invocation..
- `FORCE_MODEL_RUNNER`: Force usage of MODEL_RUNNER, for example to override `llamafile...` with llama.cpp set in in `MODEL_RUNNER`
- `THREADS`: The number of threads to use for the model. Defaults to the number of CPU cores.
- `NGL`: Number of GPU layers, same as `--ngl`
- `GPU`: auto, none, nvidia, ...
- `PRIORITY`: speed|length|manual controlling balance of GPU memory and context length
- `GRAMMAR_FILE`: Same as `--grammar-file`
- `DEBUG`: same as --debug
- `VERBOSE`: same as --verbose
- `KEEP_PROMPT_TEMP_FILE`: in what case to keep the prompt text file: NONE, ERROR, or ALL

See [env.sh.example](env.sh.example).

## Open API Usage
You can skip using a local `GGUF` or `llamafile` executable and use an Open API compatible LLM server.

Do this by setting `MODEL_TYPE` environment variable or CLI flag `--model-type` to `'via-api'`.  If the server is not local, set the environment variable `VIA_API_CHAT_BASE`, which defaults to `http://localhost:5000`.

## Text Generation WebUI API Server
You can run an Open API HTTP server on port 5000 with text-generation-webui (see #References). Additionally, the [scripts/via-api.sh](via-api.sh) CLI tool provides access to server-specific commands such as model loading and unloading.

## Program Flow
1. If there are any arguments, `--` or any non-hyphen word, terminate the arguments and start the question. 
1. If there are no hyphens to start, assume the whole arguments is a question.
1. Parse the model argument and load the model definition
1. If stdin has content, include it in a block.
1. Alternatively, if the standard input is a terminal and if so, prompt the user to give input followed by Ctrl-D.
1. Calculate the prompt length estimate and set the memory allocation for the prompt.
1. If there is a GPU, estimate the free VRAM and sets the maximum number of gradient layers accordingly.
1. Context length and the number of gradient layers based on the priority mode (speed, length, or manual).
1. Finally, perform inference using the specified model and parameters.

# Mac Specifics
You will need to do this on MacOS:
```
$ cd ~/wip/llamafiles
$ xattr -dr com.apple.quarantine models/*.llamafile models/*/*.llamafile lib/*
``

# References
## Used directly
- https://github.com/Mozilla-Ocho/llamafile 
- https://github.com/klotz/llamafiles
- https://justine.lol/oneliners/

## Similar packages
Many of these are better than this package. Please try them out.
- https://github.com/rendezqueue/rendezllama
- https://llm.datasette.io/
- https://github.com/simonw/llm-cmd
- https://github.com/jart
- https://github.com/llimllib/gpt-bash-cli
- https://github.com/yusufcanb/tlm

## Compatible Tools
- https://github.com/oobabooga/text-generation-webui

# TODO
- prompt caching
- quoting safety

