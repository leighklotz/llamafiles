#!/bin/bash

cd ~/wip/llamafiles/models/

# small models
wget https://huggingface.co/jartine/rocket-3B-llamafile/resolve/main/rocket-3b.Q5_K_M.llamafile?download=true-O rocket/rocket-3b.Q5_K_M.llamafile
wget https://huggingface.co/jartine/rocket-3B-llamafile/resolve/main/rocket-3b.Q6_K.llamafile?download=true  -O rocket/rocket-3b.Q6_K.llamafile

wget https://huggingface.co/jartine/Mistral-7B-Instruct-v0.2-llamafile/resolve/main/mistral-7b-instruct-v0.2.Q5_K_M.llamafile?download=true -O mistral/mistral-7b-instruct-v0.2.Q5_K_M.llamafile
wget https://huggingface.co/jartine/Mistral-7B-Instruct-v0.2-llamafile/resolve/main/mistral-7b-instruct-v0.2.Q6_K.llamafile?download=true -O mistral/mistral-7b-instruct-v0.2.Q6_K.llamafile

wget https://huggingface.co/jartine/phi-2-llamafile/resolve/main/phi-2.Q6_K.llamafile?download=true -O phi/phi-2.Q5_K_M.llamafile
wget https://huggingface.co/jartine/phi-2-llamafile/resolve/main/phi-2.Q6_K.llamafile?download=true -O phi/phi-2.Q6_K.llamafile

# big models
if [ 1 == 0 ];
then
   wget https://huggingface.co/jartine/Mixtral-8x7B-Instruct-v0.1-llamafile/resolve/main/mixtral-8x7b-instruct-v0.1.Q5_K_M.llamafile?download=true -O mixtral-8x7b-instruct-v0.1.Q5_K_M.llamafile
   wget https://huggingface.co/jartine/dolphin-2.5-mixtral-8x7b-llamafile/resolve/main/dolphin-2.5-mixtral-8x7b.Q4_K_M.llamafile?download=true =-O dolphin-2.5-mixtral-8x7b.Q4_K_M.llamafile
fi

# image models [needs more files]
if [ 1 == 0 ];
then
   wget https://huggingface.co/jartine/llava-v1.5-7B-GGUF/resolve/main/llava-v1.5-7b-q4.llamafile?download=true -O llava-v1.5-7b-q4.llamafile
fi
