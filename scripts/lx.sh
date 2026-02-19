#!/usr/bin/env bash
set -euo pipefail

# lx.sh - minimal reliable wrapper: print files inside fenced code blocks.
# Fixes: skip non-regular files (directories), avoid indexing associative array
# with empty keys, safer stdin collection.
# Inspired by a subset of <https://github.com/rasros/lx> but vibe coded in bash

declare -A ext_langs=(
  [go]=go [py]=python [js]=javascript [jsx]=jsx
  [ts]=typescript [tsx]=tsx [rs]=rust [java]=java
  [c]=c [h]=c [cpp]=cpp [cc]=cpp [cxx]=cpp [hpp]=cpp
  [sh]=bash [zsh]=zsh [rb]=ruby [php]=php
  [html]=html [htm]=html [css]=css [json]=json [yml]=yaml [yaml]=yaml
  [toml]=toml [md]=markdown [markdown]=markdown [txt]=text
)

before=""   # override with --before
after=""    # override with --after

usage() {
  cat <<EOF
Usage: lx.sh [--before=STR] [--after=STR] [files...]
Reads file paths from args and/or stdin (one per line).
Placeholders supported in --before: {filename}, {language}
EOF
}

# parse simple flags
while [[ $# -gt 0 ]]; do
  case "$1" in
    --before=*) before="${1#*=}"; shift;;
    --after=*) after="${1#*=}"; shift;;
    --help) usage; exit 0;;
    --) shift; break;;
    -*) echo "Unknown option: $1" >&2; usage; exit 1;;
    *) break;;
  esac
done

# collect files from args and stdin
files=("$@")
if [ ! -t 0 ]; then
  while IFS= read -r line; do
    files+=("$line")
  done
fi

if [ ${#files[@]} -eq 0 ]; then
  echo "lx: provide one or more file paths via args or stdin" >&2
  exit 1
fi

default_after=$'```\n---\n'

for f in "${files[@]}"; do
  if [ -z "$f" ]; then
    # skip empty lines from stdin
    continue
  fi

  if [ ! -e "$f" ]; then
    echo "lx: stat $f: no such file" >&2
    continue
  fi

  if [ ! -f "$f" ]; then
    echo "lx: $f is not a regular file; skipping" >&2
    continue
  fi

  if [ ! -r "$f" ]; then
    echo "lx: cannot read $f" >&2
    continue
  fi

  # determine language from extension, only if extension part is non-empty
  lang=""
  if [[ "$f" == *.* ]] && [[ "${f##*.}" != "" ]]; then
    ext="${f##*.}"
    ext="$(tr '[:upper:]' '[:lower:]' <<<"$ext")"
    lang="${ext_langs[$ext]:-}"
  fi

  # build and print before block (with placeholders)
  default_before="# file ${f}\n\`\`\`${lang}\n"
  bf="${before:-$default_before}"
  # safe placeholder replacements
  bf="${bf//\{filename\}/$f}"
  bf="${bf//\{language\}/$lang}"
  printf '%b' "$bf"

  # print file contents
  cat -- "$f"

  # print after block (default is closing fence + blank line)
  aft="${after:-$default_after}"
  printf '%b' "$aft"
done
