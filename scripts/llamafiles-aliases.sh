# use `builtin help` if you want native bash help command
help () 
{ 
    exec 3>&1;
    LLM_RESPONSE="$(help.sh "$@" | tee >(cat 1>&3))";
    local status=${PIPESTATUS[0]};
    exec 3>&-;
    return "$status"
}

ask () 
{ 
    exec 3>&1;
    LLM_RESPONSE="$(ask.sh "$@" | tee >(cat 1>&3))";
    local status=${PIPESTATUS[0]};
    exec 3>&-;
    return "$status"
}

response () {
    printf "%s\n" "${LLM_RESPONSE}"
}
