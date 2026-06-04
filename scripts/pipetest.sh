pipetest () 
{ 
    local user_query="$*";
    local tmpfile;
    if ! tmpfile=$(mktemp --tmpdir="$(mktemp -d)" pipetest.XXXXXX 2> /dev/null); then
        printf "pipetest: could not create temporary file\n" 1>&2;
        exit 1;
    fi;
    trap 'rm -f "$tmpfile"' EXIT;
    cat > "$tmpfile";
    
    local reply;
    local pager;

    # Auto-detect the best available viewing tool
    if [ -n "${PAGER}" ]; then
        pager="${PAGER}"
    elif command -v batcat >/dev/null 2>&1; then
        pager="batcat --style=numbers,grid"
    elif command -v bat >/dev/null 2>&1; then
        pager="bat --style=numbers,grid"
    elif command -v less >/dev/null 2>&1; then
        pager="less -R"
    else
        pager="cat"
    fi;
    
    # Render file content directly to stderr
    ${pager} "$tmpfile" 1>&2
    
    # Safe interactive prompt from /dev/tty (avoids the racing subshell read)
    read -r -p "🤖 ${user_query}: Y or N? " reply < /dev/tty;
    
    printf "\n" 1>&2;
    case "${reply,,}" in 
        y*)
            cat "$tmpfile"
        ;;
        *)
            printf "🚫 discarded\n" 1>&2
        ;;
    esac
}
