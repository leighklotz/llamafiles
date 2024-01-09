#!/bin/bash
# rename-pictures.sh
# Author: Justine Tunney <jtunney@gmail.com>
# License: Apache 2.0
#
# This shell script can be used to ensure all the images in a folder
# have good descriptive filenames that are written in English. It's
# based on the Mistral 7b and LLaVA v1.5 models.
#
# For example, the following command:
#
#     ./rename-pictures.sh ~/Pictures
#
# Will iterate recursively through the specified directories. For each
# file, it'll ask the Mistral model if the filename looks reasonable. If
# Mistral doesn't like the filename, then this script will ask LLaVA to
# analyze the picture and generate a new filename with lowercase letters
# and underscores. Most image formats are supported (e.g. png/jpg/gif)
# and newer more exotic ones (e.g. webp) are also supported if Image
# Magick is installed.
#
# You need to have a system with at minimum 8gb of RAM. This will work
# even on older computers without GPUs; just let it run overnight!

# [KLOTZ Fri Jan  5 12:11:36 PM PST 2024] Updates
# - removed filename check and just append to existing filename
# - changed to print script instead of act
# TODO:
# - Use remote or local model transparently

LLAVA_MODEL=~klotz/wip/llamafiles/llava-v1.5-7b-q4-main.llamafile
MISTRAL_MODEL=~klotz/wip/llamafiles/mistral-7b-instruct-v0.1-Q4_K_M-main.llamafile
#MISTRAL_MODEL=~klotz/wip/llamafiles/mistral-7b-instruct-v0.2.Q5_K_M.llamafile
NGL=35

abort() {
  printf '%s\n' "renaming terminated." >&2
  exit 1
}

if ! LLAVA=$(command -v ${LLAVA_MODEL}); then
  printf '%s\n' "${LLAVA_MODEL}: fatal error: update this script with the path of your llava llamafile" >&2
  printf '%s\n' "please download https://huggingface.co/jartine/llava-v1.5-7B-GGUF/resolve/main/${LLAVA_MODEL} and put it on the system path" >&2
  abort
fi

if ! MISTRAL=$(command -v ${MISTRAL_MODEL}); then
  printf '%s\n' "${MISTRAL_MODEL}: fatal error: update this script with the path of your mistral llamafile" >&2
  printf '%s\n' "please download https://huggingface.co/jartine/mistral-7b.llamafile/resolve/main/${MISTRAL_MODEL} and put it on the system path" >&2
  abort
fi

if ! CONVERT=$(command -v convert); then
  printf '%s\n' "${0##*/}: warning: convert command not found (please install imagemagick so we can analyze image formats like webp)" >&2
fi

isgood() {
    echo "no"
}

# isgood() {
#   echo ${1##*/}
#   "$MISTRAL" \
#       --temp 0 -ngl 35 \
#       --grammar 'root ::= "yes" | "no"' \
#       -p "[INST]Does the filename '${1##*/}' look like readable english text?[/INST]" \
#       --silent-prompt 2>/dev/null
# }

pickname() {
  "$LLAVA" \
      --image "$1" --temp 0.3 -ngl $NGL \
      --grammar 'root ::= [a-zA-Z]+ (" " [a-zA-Z]+)+' -n 10 \
      -p '### User: A good title for this image is...
### Assistant:' \
      --silent-prompt 2>/dev/null
}

# https://stackoverflow.com/a/30133294/1653720
shuf() {
  awk 'BEGIN {srand(); OFMT="%.17f"} {print rand(), $0}' "$@" |
    sort -k1,1n |
    cut -d ' ' -f2-
}


if [ $# -eq 0 ]; then
  printf '%s\n' "${0##*/}: fatal error: missing operand" >&2
  abort
fi

if [ x"$1" = x"--help" ]; then
  printf '%s\n' "usage: ${0##*/} PATH..."
  exit
fi

OIFS=$IFS
IFS='
'
for arg; do

  # ensure argument is a file or directory
  if [ ! -e "$arg" ]; then
    printf '%s\n' "$arg: fatal error: file not found" >&2
    abort
  fi

  # find all regular files under path argument
  for path in $(find "$arg" -type f -print0 | tr '\0' '\n' | shuf); do

    # ask mistral if filename needs renaming
    if ! answer=$(isgood "$path"); then
      printf '%s\n' "$path: fatal error: failed to ask mistral if file needs renaming" >&2
      abort
    fi

    if [ "$answer" = "yes" ]; then
      printf '%s\n' "skipping $path (mistral says ''${path##*/}' is good)" >&2
      continue
    fi

    # ask llm to generate new filename. if it's a format like web that
    # our stb library doesn't support yet, then we'll ask imagemagick to
    # convert it to png and then try again.
    if ! newname=$(pickname "$path"); then
      png="${TMPDIR:-/tmp}/$$.png"
      if [ -z "$CONVERT" ]; then
        printf '%s\n' "$path: warning: llava failed to describe image (probably due to unsupported file format)" >&2
        continue
      fi
      if "$CONVERT" "$path" "$png" 2>/dev/null; then
        if newname=$(pickname "$png"); then
          rm -f "$png"
        else
          printf '%s\n' "$path: warning: llava llm failed" >&2
          rm -f "$png"
          continue
        fi
      else
        printf '%s\n' "skipping $path (not an image)" >&2
        continue
      fi
    fi

    # replace spaces with underscores
    newname=$(printf '%s\n' "$newname" | sed 's/ /_/g')

    # append the original file extension to the new name
    if [ x"${path%.*}" != x"$path" ]; then
      newname="$newname.${path##*.}"
    fi

#    # prefix the original directory to the new name
#    if [ x"${path%/*}" != x"$path" ]; then
#      newname="${path%/*}/$newname"
#    fi

    # prefix the original directory and existing name to the new name
    if [ x"${path%/*}" != x"$path" ]; then
      newname="${path%.*}-$newname"
    fi

    # ensure new name is unque
    if [ -e "$newname" ]; then
      i=2
      while [ -e "${newname%.*}-$i.${newname##*.}" ]; do
        i=$((i + 1))
      done
      newname="${newname%.*}-$i.${newname##*.}"
    fi

    # rename the file
    printf 'mv %q %q\n' "$path" "$newname"
#    if ! mv -n "$path" "$newname"; then
#      printf "fatal error: failed to rename file %q:\n' "$newname" >&2
#      abort
#    fi
  done
done
IFS=$OIFS
