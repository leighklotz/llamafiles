# LLM Help CLI for Linux and Mac

Using https://github.com/Mozilla-Ocho/llamafile and https://github.com/jart models and scripts, and offering support for https://github.com/oobabooga/text-generation-webui and other API providers.

This repository provides an LLM Help CLI for Linux and Mac systems to provide help, summarization, coding assistance, system information queries, image renaming, etc. Similar to https://justine.lol/oneliners/ and other resources.

# Self Introduction
```bash
$ via --get-model-name
cognitivecomputations_dolphin-2.8-mistral-7b-v02.Q6_K.gguf
$ help.sh what can you do
```
>Here are a few things you can do with various technologies:
>
>- Linux:
>    - Operate a Linux-based operating system for desktop, server, or containerized applications.
>    - Customize and manage system settings and hardware.
>    - Install and maintain software packages and applications.
>    - Configure networking, security, and performance.
>
>- Bash:
>    - Create and run shell scripts to automate tasks or simply shorten repetitive commands.
>    - Customize the command-line interface (CLI) shell experience with scripts, shortcuts, and personalized configurations.
>    - Navigate directories, manage files and permissions, and handle data manipulation from the command line.
>
>- Python:
>    - Implement and run Python scripts for rapid application development in various fields such as web development, data analysis, machine learning, and automated tasks.
>    - Leverage Python's extensive libraries like NumPy, Pandas, Matplotlib, Django, Flask, etc. for specific tasks.
>    - Develop applications or interact with existing ones through APIs, scripts, or GUI interfaces.
>
>- General programming:
>    - Design, write, test, and debug code using various programming languages and tools.
>    - Collaborate with other developers on projects, communicate technical concepts, and contribute to open-source communities.
>    - Learn, understand, and apply programming patterns, best practices, and algorithms to solve real-world problems.
>    - Stay updated with the latest technologies, frameworks, and trends in the software industry.


## Brief Usage examples
Below are brief examples to show you what these tools do.

Instal llinks to the scripts you want to use in your home bin directory. In the example below, symlink names have been shortened.

```shell
$ help.sh -- "Split bash argument array into left and right with double hyphen as the separator using special bash builtin functions or operators"
$ help.sh what is my ip
$ help.sh -m dolphin 'Give me a JQ cli to get the value of the field named `llama.context_length`'
$ ask dolphin ...
$ man xxd | help.sh "print file contents with xxd without the address column"
$ help-commit --oneline
$ codeblock jq cat foo.jq | help.sh '"Explain this:'
$ codeblock xslt cat redir/target.xslt | help.sh explain
$ write.sh write.sh find and list empty directories in .
$ summarize https://graflex.org --via api | ask nuextract '{ "link": "", "title": "", "summary": "", "keywords": ["", "", ...] }'
$ ls -l | ask nuextract '[{ "filename": "", "metadata": { ... } }, ...]'
```

More examples can be found below and in the [examples](examples) directory.

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
klotz@rpi5:~ $ export MODEL_TYPE=rocket
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

More examples are in the [examples](examples) directory.

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

## write.sh examples
The `write.sh` command is similar in spirit to the iTerm2 AI command feature which https://gitlab.com/gnachman/iterm2/-/issues/11475
But it is not hardwared to OpenAI and does not execute the code. It differs from `help.sh` only in the default prompt.

```bash
$ write.sh show git commit message for 70eab9e
# Set the Git repository directory to /path/to/your/repo
# Change this to the actual path of your Git repository
cd /path/to/your/repo

# Show the commit message for the commit with SHA-1 70eab9e
git show 70eab9e --pretty=%s</s>
$ 
  ```

# ask examples

Ask uses a brief system prompt and allows you to conveniently/ direct your query to a specific local model, or just use
`any` for the model name in the `-via api` case.

```
ask phi3 2+3=
```

or
```
export VIA=api
ask any 2+3=
```

## ask nuextract to convert to JSON
You can use ask with the `nuextract` model, which provides its own system prompt.


The nuextract model is limited to 2048 token context, so it's best used in pipelines, e.g.:

```bash
$ summarize https://graflex.org --via api | ask nuextract '{ "link": "", "title": "", "summary": "", "keywords": ["", "", ...] }'

{
    "link": "Graflex.org",
    "title": "Graflex.org is a website dedicated to Graflex Speed Graphics and other classic large-format cameras",
    "summary": "The site aims to promote their use and preservation",
    "keywords": [
        "Graflex",
        "Speed Graphics",
        "classic",
        "large-format",
        "cameras",
        "photography",
        "Articles",
        "Help Board",
        "Q&A",
        "Camera Information",
        "Technical Information",
        "Related Websites",
        "News"
    ]
}
```

With the nuextract model  limited (DWIM)[https://en.wikipedia.org/wiki/DWIM] seems to work for the JSON structure.
Feel free to try with regular instruct-trained models as well.

```bash
$ git log -3 | ask nuextract 'appropriate JSON structure'

{
    "commit": [
        {
            "sha": "dbf2ab43732fc364539bb5d7fcc5f7c0f16ddac9",
            "author": "Leigh L. Klotz, Jr.",
            "date": "Thu Aug 22 15:03:28 2024 -0700",
            "message": "merge pull request #9 from leighklotz/nemo"
        },
        {
            "sha": "ea62e09c37b599c9c1067f7dc507fecbec7226f9",
            "author": "Leigh Klotz",
            "date": "Thu Aug 22 15:02:32 2024 -0700",
            "message": "nemo: split out nemo fom mistral"
        },
        {
            "sha": "4c4202f3bd93f459add366f0094890d615a9c8fd",
            "author": "Leigh Klotz",
            "date": "Thu Aug 22 14:11:35 2024 -0700",
            "message": "nuextract: add example with 'ls'"
        }
    ]
}
```




# llm_el for Emacs
There are many LLM integrations for Emacs; mine is here: [llm_el](llm_el).

# LLamafile binaries
Get these files from (https://github.com/Mozilla-Ocho/llamafile)[https://github.com/Mozilla-Ocho/llamafile].
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
These files are in [scripts](scripts). You might want to symlink some to your bin directory.
You can also copy [scripts/env.sh.example](scripts/env.sh.example) to `scripts/env.sh` and edit it to set default values.

## base
- llm.sh - the base script that others call

## user programs
- help.sh - CLI for Linux help - cann't be shortened to 'help' in bash
- ask.sh - like 'help.sh -m $1' - can be shortened to 'ask' in bash
- machelp.sh - CLI for Mac help
- summarize.sh - CLI to summarize a hyperlink / use '-' for stdin
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
  `--raw-input`: prepare for pure completion, ignoring PROMPT and SYSTEM_MESSAGE in favor of stdin
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

In addition to these command line flags, the script also checks for several environment variables to configure its behavior. Variables with effect in CLI-only are marked as such.

- `VIA`: The type of model runner, `api` or `cli`. Also see `--via` flag.
- `MODEL_TYPE`: The default model type to use if none is specified via the `-m` or `--model-type` flag. (cli)
- `TEMPERATURE`: The default temperature parameter for the model if none is specified via the `--temperature` flag.
  `TOP_K`: TOP_K. Currently only set for api models.
  `TOP_P`: TOP_P. Currently only set for api models.
  `MIN_P`: MIN_P. Currently only set for api models.
- `CONTEXT_LENGTH`: The default context length for the model if none is specified via the `--context-length` or `-c` flag. (cli)
- `N_PREDICT`: The default number of tokens to predict if none is specified via the `--n-predict` flag.
- `SYSTEM_MESSAGE`: The default system message to use if none is specified via the command line.
- `MODEL_RUNNER`: The program used to run the model. Defaults to `/usr/bin/env` and automatically set to `llamafile...` if needed. See also `FORCE_MODEL_RUNNER` (cli)
- `LLM_ADDITIONAL_ARGS`: Value is interpolated into the call to LLM at the end of the CLI invocation. (cli)
- `FORCE_MODEL_RUNNER`: Force usage of MODEL_RUNNER, for example to override `llamafile...` with llama.cpp set in in `MODEL_RUNNER` (cli)
- `THREADS`: The number of threads to use for the model. Defaults to the number of CPU cores. (cli)
- `NGL`: Number of GPU layers, same as `--ngl` (cli)
- `GPU`: auto, none, nvidia, ... (cli)
- `PRIORITY`: speed|length|manual controlling balance of GPU memory and context length (cli)
- `GRAMMAR_FILE`: Same as `--grammar-file`
- `DEBUG`: same as --debug
- `VERBOSE`: same as --verbose
- `KEEP_PROMPT_TEMP_FILE`: in what case to keep the prompt text file: NONE, ERROR, or ALL

See [env.sh.example](env.sh.example).

## Basic Inference Script

The script [scripts/llm.sh](scripts/llm.sh) controls almost text-based access to LLamafiles and to OpenAPI inference. 

Choose a model type (API or Local LLama) by by setting environment variable `export VIA=api` or `export VIA=cli`,  or `llm.sh` flag flag `--via api`.

You can control the default model by specifying environment variable `$MODEL_TYPE`, for example in your shell or in [scripts/env.sh](scripts/env.sh). If `$MODEL_TYPE` or the `-m ` argument to `llm.sh` is one of the model types in [models](models), then `llm.sh` will use the largest executable `.llamafile` or GGUF file.

Todo: Document LLAMAFILE_RUNNER here.

The [scripts/via.sh](via.sh) CLI tool provides access to server-specific commands, such as listing models and model types.
Although it can use Oobabooga/text-generation-webui API to remotey load models in the `--via api` case,  there is no facility to `load` a LLamafile model in the `--via cli` case; instead set `MODEL_TYPE` or use the `-m` flag.

## Open API Usage
You can skip using a local `GGUF` or `llamafile` executable and instead use an Open API compatible LLM server.

Do this by setting `VIA` environment variable or CLI flag `--via` to `api'.  If the server is not local, set the environment variable `VIA_API_CHAT_BASE`, which defaults to `http://localhost:5000`.

You can run `llamafiles` in server mode with [scripts/start-server.sh](scripts/start-server.sh) or by using Oobabooga/text-generation-webui (see #References).

The [scripts/via.sh](via.sh) CLI tool provides access to server-specific commands, such as model loading and unloading.

## Image Usage
Image pipelines are handled solely by @jartine https://justine.lol/oneliners/ and https://github.com/Mozilla-Ocho/llamafile directly, with a few minor changes copied here for convenience, and are not yet integrated into the LLM.sh.

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

# Installation
Case 1: If you want to use links or lynx to fetch web pages, install them first.

```
$ sudo apt install linx lynx
$ ./scripts/install-scripts.sh ~/bin
```

Case 2: Install the scripts for playwright headless browser usage:

If you want to use playwright to fetch web pages, give the `--downlink` final argument to `install-scripts.sh`
```bash
$ ./scripts/install-scripts.sh ~/bin --downlink
```

## Mac Specifics
You will need to do this on MacOS:
```
$ cd ~/wip/llamafiles
$ xattr -dr com.apple.quarantine models/*.llamafile models/*/*.llamafile lib/*
```

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
- https://github.com/ErikBjare/gptme
- https://github.com/llimllib/gpt-bash-cli
- https://github.com/yusufcanb/tlm
- https://github.com/noemaresearch/pinboard

### Similar Emacs Packages
- https://github.com/chep/copilot-chat.el

## Compatible LLM Inference Providers
- https://github.com/oobabooga/text-generation-webui
- https://github.com/ggerganov/llama.cpp
- https://github.com/Mozilla-Ocho/llamafile
- OpenAI API - partial support

## Special-purpose LLM Inference models
- https://huggingface.co/numind/NuExtract

# TODO
- prompt caching
- quoting safety
