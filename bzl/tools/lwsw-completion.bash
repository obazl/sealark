#!/usr/bin/env bash

while (( "$#" )); do
    case "$1" in
        -*|--*=) # flags
        FLAGS="$FLAGS $1"
        shift
        ;;
        *) # preserve positional arguments
            PARAMS="$PARAMS $1"
            shift
            ;;
    esac
done

# set positional arguments in their proper place
eval set -- "$PARAMS"

_comp_lsws()
{
    local IFS=$' \t\n'    # normalize IFS

    output_base="$(bazel info output_base)/external"

    if [[ $2 == "" ]]; then
        COMPREPLY=( $(compgen -W ${output_base} -- $2) )
    else
        COMPREPLY=( $(compgen -d -- $2) )
        COMPREPLY+=( $(compgen -f -- $2) )
    fi

    return 0
}

complete -o filenames -o nospace -F _comp_lsws lsws
