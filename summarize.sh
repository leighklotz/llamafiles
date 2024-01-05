#!/bin/bash

URL="${1}"
PROMPT="Summarize the following article:\n"
TEXT="$(links -codepage utf-8 -force-html -width 72 -dump ${URL})"
#echo "${TEXT:0:128}"

#TEXT="$(echo $'[INST]Summarize the following article:\n'; links -codepage utf-8 -force-html -width 72 -dump "${URL}" | cut -c1-5800; echo $'\n[/INST]\n')"
#TEXT="$(echo $'<|im_start|>Summarize the following article in a maximum of 3 bullet points:\n'; cat article.txt; echo $'\n<|im_end|>\n')"
#echo $TEXT
/home/klotz/wip/llamafiles/mistral-7b-instruct-v0.1-Q4_K_M-main.llamafile --temp 0 -n 1000 -c 6000 -p "[INST]${PROMPT}${TEXT:0:5900}[/INST]" --silent-prompt 2>/dev/null 

