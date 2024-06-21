_via() {
    local cur prev opts models
    COMPREPLY=()
    cur=${COMP_WORDS[COMP_CWORD]}
    prev=${COMP_WORDS[COMP_CWORD-1]}
    
    opts=(
        "--get-via"
        "--get-model-name"
        "--list-models"
        "--list-models-types"
        "--load-model"
        "--unload-model"
        "--via"
        "--help"
    )
    
    if [[ ${prev} == "--load-model" ]]; then
        models=$(via --list-models)
        COMPREPLY=( $(compgen -W "${models}" -- ${cur}) )
        return 0
    elif [[ ${prev} == "--via" ]]; then
        vias="api cli"
        COMPREPLY=( $(compgen -W "${vias}" -- ${cur}) )
        return 0
    elif [[ ${cur} == -* && ${COMP_CWORD} -eq 1 ]]; then
        COMPREPLY=( $(compgen -W "${opts[*]}" -- ${cur}) )
        return 0
    fi
}

complete -F _via via
