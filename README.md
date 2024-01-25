# LLM Help CLI for Linux
- using https://github.com/Mozilla-Ocho/llamafile and scripts from https://github.com/jart

# Script Files
## base
- llm.sh
- server.sh

## user programs
- help.sh
- machelp.sh
- summarize.sh

## images
- image-name.sh
- rename-pictures.sh [adapted from https://gist.github.com/jart/bd2f603aefe6ac8004e6b709223881c0]
- llava-cli.sh 

## utilities
- systype.sh
- nvfree.sh

## help.sh
USAGE="[-m|--model-type model-type] [--stdin] [--speed | --length] [--temperature temp] [--context-length|-c n] [--ngl n] [--n-predict n] [--debug] [--verbose] [--] QUESTION*"

- `-m`, `--model-type`: defaults if not specified; see source for model types
- `--stdin`: reads stdin and interpolates into prompt; if input is a terminal, asks user for input
- `--`: unambiguously terminate args
- `QUESTION`: word or words, can be quoted or not, can be multiline (but also see `--stdin`)
- [other options]: See source
- $ENV: see source

## Examples
Below are a few. More are in [examples](examples).

### Bash Coding Example
```
$ help.sh -m codebooga -- "split bash argument array into left and right with double hyphen as the separator using special bash builtin functions or operators as needed "
```

### General Linux Example
```
$ help.sh what is my ip address
```

### JQ Example
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

Proof it works:
```
$ ./llama.cpp/gguf-py/scripts/gguf-dump.py ./models/dolphin-2.7-mixtral-8x7b.Q4_K_M.gguf --no-tensors --json | jq '.metadata["llama.context_length"].value'
    32768
$
```

### LSHW Example
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

### Raspberry Pi 5 lspci with Rocket model
```
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

# Models
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

### Desktop and GPU models
These go in `models/`:
- `dolphin-2.5-mixtral-8x7b.Q4_K_M.llamafile`
- `llava-v1.5-7b-q4-main.llamafile`
- `llava-v1.5-7b-q4-server.llamafile`
- `mistral-7b-instruct-v0.2.Q4_K_M.llamafile`

### RPI5 and other small models
These go in `models/`:
- `mistral-7b-instruct-v0.2.Q3_K_M.llamafile`
- `mixtral_7bx2_moe.Q3_K_M.gguf`
- `phi-2.Q5_K_M.llamafile`
- `phi-2.Q6_K.llamafile`
- `rocket-3b.Q4_K_M.llamafile`

# Mac Specifics
- `xattr -dr com.apple.quarantine models/* bin/*`
