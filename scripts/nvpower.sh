#!/bin/bash -e

set -o pipefail

# todo: implement --set
usage() {
    echo "Usage: $0 {--draw|--limit} | --set <value>"
    echo "    --draw  : Display the current power draw in Watts"
    echo "    --limit : Display the maximum power limit in Watts"
    echo "    --set <value> : Set the maximum power limit in Watts. Requires root privileges."
}

if command -v nvidia-smi &> /dev/null; then
    POWER_DRAW=$(nvidia-smi --query-gpu=power.draw --format=csv,nounits,noheader | awk '{print $1 * 1}')
    POWER_LIMIT=$(nvidia-smi --query-gpu=power.limit --format=csv,nounits,noheader | awk '{print $1}')

    if [ "$#" -eq 0 ]; then
        echo "${POWER_DRAW}/${POWER_LIMIT}"
        exit 0
    fi

    case "$1" in
        --draw)
            echo "$POWER_DRAW"
            ;;
        --limit)
            echo "$POWER_LIMIT"
            ;;
        --set)
            if [ -z "$2" ]; then
                echo "Error: --set requires a value."
                usage
                exit 1
            fi

            # Validate input: check if $2 is a number
            if ! [[ "$2" =~ ^[0-9]+$ ]]; then
                echo "Error: Invalid power limit value. Must be a number."
                usage
                exit 1
            fi

            sudo nvidia-smi --power-limit="$2"
            if [ $? -eq 0 ]; then
                echo "Power limit set to $2 Watts."
            else
                echo "Error: Failed to set power limit to $2 Watts."
                exit 1
            fi
            ;;
        *)
            usage
            exit 1
            ;;
    esac
else
    echo "0"
    exit 1
fi
