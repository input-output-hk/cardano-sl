#!/usr/bin/env bash

# Optional path for `cardano-sl`
cardano_path=${1:-../cardano-sl}

./scripts/run.sh ../cardano-sl/scripts/common.sh & PIDEX=$!
WALLET_TEST=1 $cardano_path/scripts/launch.sh & PIDNODE=$!
wait $PIDEX
wait $PIDNODE