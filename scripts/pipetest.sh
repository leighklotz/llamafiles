# pipetest alias - forward piped data after a Y/N confirmation
# Read all data from standard input (the "pipe").
# Ask the user "Y or N? " (case‑insensitive, newline required).
# If the answer starts with "y" (or "yes") the data is forwarded
# to stdout.  Otherwise nothing is written.
function pipetest() {
    local tmpdir tmpfile
    if ! tmpdir=$(mktemp -d 2>/dev/null) ; then
        printf >&2 "pipetest: could not create temporary directory\n"
        return 1
    fi
    trap 'rm -rf "$tmpdir"' EXIT
    if ! tmpfile=$(mktemp --tmpdir="$tmpdir" pipetest.XXXXXX 2>/dev/null) ; then
        printf >&2 "pipetest: could not create temporary file\n"
        return 1
    fi

    # 2. Read all of stdin into the temp file.
    cat >"$tmpfile"
    
    local reply
    local pager

    # Auto-detect the best available viewing tool
    if [ -n "${PIPETEST_PAGER}" ]; then
        pager="${PIPETEST_PAGER}"
    elif command -v batcat >/dev/null 2>&1; then
        pager="batcat --style=numbers,grid"
    elif command -v bat >/dev/null 2>&1; then
        pager="bat --style=numbers,grid"
    elif command -v less >/dev/null 2>&1; then
        pager="less -R"
    else
        pager="cat"
    fi
    
    # Render file content directly to stderr
    ${pager} "$tmpfile" 1>&2
    
    # Safe interactive prompt from /dev/tty (avoids the racing subshell read)
    read -r -p "🤖 ${user_query}: Y or N? " reply < /dev/tty
    
    printf "\n" 1>&2
    case "${reply,,}" in 
        y*)
            cat "$tmpfile"
        ;;
        *)
            printf "🚫 discarded\n" 1>&2
        ;;
    esac
}
