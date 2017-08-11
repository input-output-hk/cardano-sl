#!/usr/bin/env bash
set -e
set -o pipefail

echo "Cleaning all databases and artifacts..."

echo "Are you sure you want to remove .stack-work directory? You will have to rebuild Cardano SL completely. Type 'yes' to continue..."
read DECISION
if [ "${DECISION}" == "yes" ]; then
    echo "Cleaning Cardano SL stack-work..."
    rm -rf .stack-work
    ./scripts/clean/db.sh
    ./scripts/clean/daedalus-bridge.sh
    ./scripts/clean/explorer-bridge.sh
    exit 0
else
    echo "Abort.";
    exit 1
fi
