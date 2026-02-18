#!/bin/bash

# This script symlinks useful shell scripts to a destination directory
# It tries to add the destination directory to your .bashrc
# e.g.: ./install-scripts.sh ~/wip/llamafiles/bin

SCRIPT_DIR="$(dirname "$(realpath "${BASH_SOURCE}")")"

FILES="ask.sh bashblock.sh codefence.sh help-commit.sh lx.sh machelp.sh manhelp.sh nvfree.sh onsubnet.sh repofiles.sh scuttle.sh summarize.sh systype.sh unfence.sh unlx.sh via.sh"
SH_FILES="help.sh write.sh"

DEST_DIR=""

BASH_D="$HOME/.bash.d"
BASHRC="$HOME/.bashrc"

usage() {
  echo "Usage: $0 <dest-dir>"
  echo ""
  echo "  <dest-dir>      The destination directory to symlink scripts to."
  echo ""
  echo "Example: $0 ~/bin"
  exit 1
}

# Parse CLI options
while [[ $# -gt 0 ]]; do
  case "$1" in
    --help)
      usage
      ;;
    *)
      if [ -z "$DEST_DIR" ]; then
        DEST_DIR="$1"
        DEST_DIR="$(realpath "$1")"
      else
        echo "Error: Too many arguments."
        usage
      fi
      shift
      ;;
  esac
done

if [ -z "${DEST_DIR}" ]; then
    usage
fi

function lnf {
    local src="$1"
    local dst="$2"
    if [ -z "$src" ] || [ -z "$dst" ] ;
    then
	echo "fail: lnf src=$src dst=$dst"
	exit 1
    fi
    if [ ! -e "${dst}" ]; then
	echo "* ln -s ${src} ${dst}"
	ln -s "${src}" "${dst}"
    else
	#echo "${dst}" exists
        true # Suppress output if file exists.
    fi
}

mkdir -p "${DEST_DIR}"

for file in ${FILES}
do
    lnf "${SCRIPT_DIR}/${file}" "${DEST_DIR}/${file%.sh}"
done

for file in ${SH_FILES}
do
    lnf "${SCRIPT_DIR}/${file}" "${DEST_DIR}/${file}"
done

# Add $DEST_DIR to PATH in .bashrc if it's not already there
if ! grep -q "$DEST_DIR" ~/.bashrc; then
  echo "* Adding $DEST_DIR to PATH in ~/.bashrc"
  echo "export PATH=\"$DEST_DIR:\$PATH\"" >> ~/.bashrc
fi

# Install aliases into ~/.bash.d
# Add support for ~/.bash.d to ~/.bashrc if we created ~/.bash.d
if [[ ! -d "$BASH_D" ]]; then
  mkdir -p "$BASH_D"
  echo "* Created $BASH_D"
  source_line='for f in "$HOME"/.bash.d/*.sh; do [ -r "$f" ] && . "$f"; done'
  printf '\n# Source per-user scripts in ~/.bash.d\n%s\n' "$source_line" >> "$BASHRC"
  echo "* Appended ~/.bash.d sourcing line to $BASHRC"
fi

lnf "${SCRIPT_DIR}/llamafiles-aliases.sh" "$BASH_D/llamafiles-aliases.sh"
lnf "${SCRIPT_DIR}/_via_completion.sh" "$BASH_D/_via_completion.sh"

# Check for yq (https://github.com/mikefarah/yq/) version v4.49.2
if ! command -v "yq" >/dev/null 2>&1; then
    echo "* You need to install yq"
    exit 1
fi
