# LLM Help CLI for Linux and Mac

This repository provides an LLM Help CLI for Linux and Mac systems to provide help, summarization, coding assistance, system information queries, etc., using OpenAPI providers.


# Self Introduction
Exact capabilities depend on the model used. Below is a sample interaction.

```bash
$ via --get-model-name
unsloth_gemma_4_26B_A4B_it_UD_Q4_K_M
$ help --think what can you do?
I can assist with a wide range of tasks, including:

*   **Programming:** Writing, debugging, and optimizing code in languages such as Python, Bash, C++, JavaScript, etc.
*   **Linux/Systems Administration:** Explaining commands, writing shell scripts, managing permissions, and configuring environments.
*   **Technical Support:** Answering questions about computer science concepts, data structures, and algorithms.
*   **Data Processing:** Writing scripts for data manipulation, parsing files, and automation.
*   **General Knowledge:** Providing information on diverse subjects including mathematics, science, history, and literature.
*   **Content Creation:** Generating documentation, README files, and technical explanations.
```

## Brief Usage examples
Examples can be found below and in the [examples](examples) directory.

## help usage and examples
USAGE="[--stdin|--interactive|-i] [--temperature temp] [--n-predict n] [--debug] [--verbose] [--] QUESTION*"

Uses question words and stdin, if any, to create the model prompt.

- `--stdin|--interactive|-i`: if input is a terminal, asks user for input and interpolates stdin into prompt; defaults to on if used in a pipe
- `--`: unambiguously terminate args
- `QUESTION`: word or words, can be quoted or not, can be multiline (but also see `--stdin`)
- [other options]: See source
- $ENV: see env.sh.examples or source of llm.sh

Below are a few examples. More are in [examples](examples).

### help Bash Coding Example
```
$ help "split bash argument array into left and right with double hyphen as the separator using special bash builtin functions or operators as needed "
```

### help General Linux Example
```
$ help what is my ip address
```

### help JQ Example
This example takes a live JSON input and shows how to extract a slightly tricky value. As a bonus, the model gives the value of the field.

```
$ ./llama.cpp/gguf-py/scripts/gguf-dump.py ./models/dolphin/dolphin-2.7-mixtral-8x7b.Q4_K_M.gguf --no-tensors --json | \
   help 'give me a jq cli to get the value of the named `llama.context_length` (note the dot is part of the field name) in the following JSON:'

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

### help LSHW Example

```
sudo lshw | wc 
    727    2411   31214
$ sudo lshw | help 'Summarize the following lshw output:'

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

### help Raspberry Pi 5 lspci with Rocket model
```
klotz@rpi5:~ $ help lspci
LSPCI, or List PCI Devices, is a command used in Linux to display information about all the PCI devices connected to the system's motherboard. It provides details such as device vendor and product IDs, memory sizes, and supported devices. This command can be executed in the terminal of a Linux system with root privileges. For example, to list all PCI devices, you would type `lspci` in the terminal and press Enter. The output will display the information about each device.
klotz@rpi5:~ $ lspci | help "explain this lspci output"
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

This script uses llm.sh to generate commit messages from the current directory, based on `git diff` output.

```bash
$ help-commit [--quiet] [-- [llm.sh options]]

For example, to create an oneline commit message, use the following command:

```bash
help-commit
```

To show the commit message and conditionally execute the (presumably sole) code fence:
```bash
$ help-commit | pipetest | unfence | bash
```

## write examples
The `write` command differs from `help` only in the default prompt.

```bash
$ write show git commit message for 70eab9e
# Set the Git repository directory to /path/to/your/repo
# Change this to the actual path of your Git repository
cd /path/to/your/repo

# Show the commit message for the commit with SHA-1 70eab9e
git show 70eab9e --pretty=%s</s>
$ 
  ```

# llm_el for Emacs
There are many LLM integrations for Emacs; mine is here: [llm_el](llm_el).

# Scripts and Files
These files are in [scripts](scripts). You might want to symlink some to your bin directory.
You can also copy [scripts/env.sh.example](scripts/env.sh.example) to `scripts/env.sh` and edit it to set default values.

## base
- llm.sh - the base script that others call

## user programs
- ask - Like `help` but not not customized to Linux help
- bx [cmd ...] - Run command and show output in a bash codefence. You can pipe the result to `help`
- codefence [filename] - Pipe to `help` to wrap file in a codeblock
- help - CLI for Linux help - use `builtin help` to access the built-in Bash help function instead
- help-commit - CLI to run `git diff` and produce a commit message
- lx - prints multiple files inside markdown in a named, fenced code block.
- machelp - CLI for Mac help
- nvfree - check your GPU usage
- nvpower - Monitor and set NVIDIA GPU power draw and limits
- onsubnet - Check if the local IP address matches a specific subnet prefix
- scuttle - Summarize a webpage into a bookmark link or YAML format
- summarize-directory-files.sh - summarize directory files as markdown
- summarize - CLI to summarize a hyperlink / use '-' for stdin
- systype - Pipe to `help` to provide context for distro-specific questions
- unfence - Pipe response to `unfence` to extract content, usually code, between triple-backquotes.
- unlx - Extract a specific file's content from an `lx` formatted archive (stdin to stdout)

# llm.sh Details
The help and summary scripts invoke `llm.sh`, but you can use it yourself directly as well.

## Command Line Flags
`llm.sh` accepts the following command line flags:

- `--temperature`: sets the temperature of the model.
- `--verbose`: enables verbose mode.
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
$ help 'How can I use the `yes` command in bash?'
```
but this is bad:
```bash
$ help "How can I use the `yes` command in bash?"
```

## Environment Variables

In addition to these command line flags, the script also checks for several environment variables to configure its behavior. Variables with effect in CLI-only are marked as such.
- See [docs/settings.md](docs/settings.md) for a full list.
- See [env.sh.example](env.sh.example) for example settings.

## Basic Inference Script

The script [scripts/llm.sh](scripts/llm.sh) offers LLM inference.

The [scripts/via.sh](via.sh) CLI tool provides access to server-specific commands, such as listing models and model types.
It uses Oobabooga/text-generation-webui API to remotely load models.

If the server is not local, set the environment variable `VIA_API_CHAT_BASE`, which defaults to `http://localhost:5000`.

The [scripts/via.sh](via.sh) CLI tool provides access to server-specific commands, such as model loading and unloading, for certain LLM inference backends.

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

If you want to use playwright to fetch web pages, install [/leighklotz/downlink](downlink)`.
```bash
$ git clone https://github.com/leighklotz/downlink
$ cd downlink
$ ./install.sh ~/.local/bin
```

Beware that playwright installs a significant number of dependencies.

# Similar packages
Many of these are better than this package. Please try them out.
- https://justine.lol/oneliners/
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
- https://github.com/ggerganov/llama.cpp - llama-server
- OpenAI API - partial support
