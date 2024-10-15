#!/bin/bash -e

set -o pipefail

usage() {
    echo "Usage: $0 {--draw|--limit}"
    echo "    --draw : Display the current power draw in Watts"
    echo "    --limit   : Display the maximum power limit in Watts"
}

if command -v nvidia-smi &> /dev/null; then
    POWER_DRAW=$(nvidia-smi --query-gpu=power.draw --format=csv,nounits,noheader | awk '{print $1 * 1} ')

    #	nvidia-smi --query-gpu=power.current --format=csv,nounits,noheader | awk '{print $1}')
    POWER_LIMIT=$(nvidia-smi --query-gpu=power.limit --format=csv,nounits,noheader | awk '{print $1}')

    if [ "$#" -eq 0 ]; then
	# todo: only do this fir 0 args case else usage error
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
        *)
            usage
            exit 1
            ;;
    esac
else
    echo "0"
    exit 1
fi
