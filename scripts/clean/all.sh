#!/usr/bin/env bash
set -e
set -o pipefail

echo "Cleaning Cardano SL stack-work..."
rm -rf .stack-work

./scripts/clean/db.sh
./scripts/clean/daedalus-bridge.sh
