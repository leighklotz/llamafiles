# LLM Help CLI for Linux

Using https://github.com/Mozilla-Ocho/llamafile and https://github.com/jart models and scripts

This repository provides an LLM Help CLI for Linux and Mac systems to provide help, summarization, coding assistance, system information queries, image renaming, etc.

## Usage examples
```shell
$ help.sh -- "Split bash argument array into left and right with double hyphen as the separator using special bash builtin functions or operators"
$ help.sh what is my ip
$ help.sh -m dolphin --stdin -- 'Give me a JQ cli to get the value of the field named `llama.context_length`'
$ man xxd | help.sh "print file contents with xxd without the address column"
```

More examples can be found in the [examples](https://github.com/leighklotz/llamafiles/tree/main/examples) directory.

## help.sh
USAGE="[-m|--model-type model-type] [--stdin|--interactive|-i] [--speed | --length] [--temperature temp] [--context-length|-c n] [--ngl n] [--n-predict n] [--debug] [--verbose] [--] QUESTION*"

Uses question words and stdin, if any, to create the model prompt.

- `-m`, `--model-type`: defaults if not specified; see source for model types
- `--stdin|--interactive|-i`: if input is a terminal, asks user for input and interpolates stdin into prompt 
- `--`: unambiguously terminate args
- `QUESTION`: word or words, can be quoted or not, can be multiline (but also see `--stdin`)
- [other options]: See source
- $ENV: see env.sh.examples or source of llm.sh

Below are a few examples. More are in [examples](examples).

# Examples

## Bash Coding Example
```
$ help.sh -m codebooga -- "split bash argument array into left and right with double hyphen as the separator using special bash builtin functions or operators as needed "
```

## General Linux Example
```
$ help.sh what is my ip address
```

## JQ Example
This example takes a live JSON input and shows how to extract a slightly tricky value. As a bonus, the model gives the value of the field.

```
$ ./llama.cpp/gguf-py/scripts/gguf-dump.py ./models/dolphin-2.7-mixtral-8x7b.Q4_K_M.gguf --no-tensors --json | help.sh -m dolphin --stdin -- 'give me a jq cli to get the value of the named `llama.context_length` (note the dot is part of the field name) in the following JSON:'

To get the value of `llama.context_length` using jq, you can use the following command:

	```
	cat file.json | jq '.metadata["llama.context_length"].value'
	```

Replace `file.json` with the path to your JSON file. This command will output the value of `llama.context_length`, which is 32768 in this case.
$  
```
Here is proof that the JQ expression given above works:

```
$ ./llama.cpp/gguf-py/scripts/gguf-dump.py ./models/dolphin-2.7-mixtral-8x7b.Q4_K_M.gguf --no-tensors --json | jq '.metadata["llama.context_length"].value'
    32768
$
```

## LSHW Example
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
	Ubuntu Linux with kernel version 6.2.0-39-generic, and has a wireless
	network interface (Intel Comet Lake PCH CNVi WiFi) and an Ethernet
	interface (Intel Ethernet Controller I225-V). The computer also has
	USB ports, audio devices, and an SMBus controller.  [end of text]
```

## Raspberry Pi 5 lspci with Rocket model
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

More examples can be found in the [examples](https://github.com/leighklotz/llamafiles/tree/main/examples) directory.

# Models and Binaries
Download your own models.

## LLamafile binaries
These go in `bin/`:

https://github.com/Mozilla-Ocho/llamafile
- `llamafile-0.2.1.zip`
- `llamafile-llava-cli-0.2.1`
- `llamafile-llava-quantize-0.2.1`
- `llamafile-main-0.2.1`
- `llamafile-quantize-0.2.1`
- `llamafile-server-0.2.1`
- `zipalign-0.2.1`

## Models and all-in-one
You need to download these from https://huggingface.co/jartine and other places on HF.

## Desktop and GPU models
These go in `models/`:
- `dolphin-2.5-mixtral-8x7b.Q4_K_M.llamafile`
- `llava-v1.5-7b-q4-main.llamafile`
- `llava-v1.5-7b-q4-server.llamafile`
- `mistral-7b-instruct-v0.2.Q4_K_M.llamafile`

## RPI5 and other small models
These go in `models/`:
- `mistral-7b-instruct-v0.2.Q3_K_M.llamafile`
- `mixtral_7bx2_moe.Q3_K_M.gguf`
- `phi-2.Q5_K_M.llamafile`
- `phi-2.Q6_K.llamafile`
- `rocket-3b.Q4_K_M.llamafile`


# Scripts and Files
## base
- llm.sh - the base script that others call

## user programs
- help.sh - CLI for Linux help
- machelp.sh - CLI for Mac help
- summarize.sh - CLI to summarize a hyperlink

## utilities
- systype.sh - Pipe to help.sh to provide context for distro-specific questions
- nvfree.sh - check your GPU usage
- code.sh [lang] [cmd]  - Pipe to help.sh to wrap output of cmd in a codeblock of type lang.

## images
These are less developed.

- image-name.sh - Simply script using LLAVA to find image name
- rename-pictures.sh [adapted from https://gist.github.com/jart/bd2f603aefe6ac8004e6b709223881c0]
- llava-cli.sh 

# llm.sh Details

## Command Line Flags

`llm.sh` accepts the following command line flags:

- `-m` or `--model-type`: specifies the type of model to use. The available options are `mixtral`, `dolphin`, `mistral`, `codebooga`, `deepseek`, `rocket`, and `phi`.
- `--speed`: sets the priority to speed.
- `--length`: sets the priority to length.
- `--temperature`: sets the temperature of the model.
- `--verbose`: enables verbose mode.
- `-c` or `--context-length`: sets the context length.
- `--ngl`: sets the number of gradient layers.
- `--n-predict`: sets the number of tokens to predict.
- `--debug`: enables debug mode.
- `--noerror`: disables error output.
- `--stdin` or `--interactive` or `-i`: enables standard input mode.
- `-e` or `--process-question-escapes`: expand \n and friends in the question; stdin is always literlal
- `--`: terminates the arguments and starts the question.

## Environment Variables

See [env.sh.example](env.sh.example).

## Program Flow
1. If there are any arguments, `--` or any non-hyphen word, terminate the arguments and start the question. 
1. If there are no hyphens to start, assume the whole arguments is a question.
1. If stdin has content, include it in a block.
1. Alternatively, if the standard input is a terminal and if so, prompt the user to give input followed by Ctrl-D.
1. Calculate the prompt length estimate and set the memory allocation for the prompt.
1. If there is a GPU, estimate the free VRAM and sets the maximum number of gradient layers accordingly.
1. Context length and the number of gradient layers based on the priority mode (speed, length, or manual).
1. Finally, perform inference using the specified model and parameters.

# Mac Specifics
You will need to do this on MacOS:
- `xattr -dr com.apple.quarantine models/* bin/*`

# References
- https://github.com/Mozilla-Ocho/llamafile 
- https://github.com/jart
- https://llm.datasette.io/
- https://github.com/klotz/llamafiles
