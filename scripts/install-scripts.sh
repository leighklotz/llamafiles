#!/bin/bash

# This script symlinks useful shell scripts to a destination directory.
# It also optionally sets up a python virtual environment for 'downlink.py'.

SCRIPT_DIR="$(dirname "$(realpath "${BASH_SOURCE}")")"

FILES="ask.sh bashblock.sh codeblock.sh help-commit.sh machelp.sh manhelp.sh nvfree.sh onsubnet.sh peas.sh repofiles.sh scuttle.sh summarize-directory-files.sh summarize.sh systype.sh unfence.sh via.sh"
SH_FILES="help.sh write.sh"

DEST_DIR=""
DOWNLINK_MODE=false

usage() {
  echo "Usage: $0 <dest-dir> [--downlink]"
  echo ""
  echo "  <dest-dir>      The destination directory to symlink scripts to."
  echo "  --downlink      Enable downlink mode: installs python virtual environment and dependencies."
  echo ""
  echo "Example: $0 ~/bin --downlink"
  exit 1
}

# Parse CLI options
while [[ $# -gt 0 ]]; do
  case "$1" in
    --help)
      usage
      ;;
    --downlink)
      DOWNLINK_MODE=true
      shift
      ;;
    *)
      if [ -z "$DEST_DIR" ]; then
        DEST_DIR="$1"
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

for file in ${FILES}
do
    lnf "${SCRIPT_DIR}/${file}" "${DEST_DIR}/${file%.sh}"
done

for file in ${SH_FILES}
do
    lnf "${SCRIPT_DIR}/${file}" "${DEST_DIR}/${file}"
done

# Install python .venv for downlink.py
if $DOWNLINK_MODE; then
    echo "* Installing downlink dependencies in ${DEST_DIR}/.venv"
    cd "${DEST_DIR}" || exit 1 # Important: cd into DEST_DIR for venv creation
    python3 -m venv .venv
    . .venv/bin/activate
    pip3 install -r "${SCRIPT_DIR}/requirements.txt" #Use SCRIPT_DIR for requirements
    playwright install
    echo "* Downlink dependencies installed in ${DEST_DIR}/.venv"
fi
