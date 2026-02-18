_via() {
    local cur prev opts models
    COMPREPLY=()
    cur=${COMP_WORDS[COMP_CWORD]}
    prev=${COMP_WORDS[COMP_CWORD-1]}

    opts=(
        "--get-model-name"
        "--list-models"
        "--load-model"
        "--unload-model"
        "--via"
        "--help"
    )

    if [[ ${prev} == "--load-model" ]]; then
        models=$(via --list-models)
        if [[ -n "$models" ]]; then
            COMPREPLY=( $(compgen -W "${models}" -- "${cur}") )
        fi
        return 0
    elif [[ ${prev} == "--list-models" ]]; then
        # Handle filtering by substring if needed
        if [[ -n "$cur" ]]; then
            models=$(via --list-models "$cur")
        else
            models=$(via --list-models)
        fi

        if [[ -n "$models" ]]; then
            local words=()
            # Split on hyphens, periods, and spaces, convert to lowercase, remove duplicates
            # Remove all-digit words
            # Remove words less than 3 characters
            while IFS= read -r word; do
                if [[ -n "$word" ]] && ! [[ "$word" =~ ^[0-9]+$ ]] && [[ ${#word} -ge 3 ]]; then
                    words+=("$word")
                fi
            done < <(echo "$models" | tr '[:space:]-.' '\n' | tr '[:upper:]' '[:lower:]' | sort -u)

            COMPREPLY=( $(compgen -W "${words[*]}" -- "${cur}") )
        fi
        return 0
    elif [[ ${cur} == -* && ${COMP_CWORD} -eq 1 ]]; then
        COMPREPLY=( $(compgen -W "${opts[*]}" -- "${cur}") )
        return 0
    fi
}

complete -F _via via
