# LLM Help CLI for Linux
- using https://github.com/Mozilla-Ocho/llamafile and scripts from https://github.com/jart

# Script Files
- cli.sh
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

Example:
```bash
$ help.sh -m codebooga -- "split bash argument array into left and right with double hyphen as the separator using special bash builtin functions or operators as needed "
$ help.sh what is my ip address
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
