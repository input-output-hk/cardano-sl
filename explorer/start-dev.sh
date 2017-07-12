#!/usr/bin/env bash

# Remove logs
rm *.log

# Optional path for `cardano-sl`
cardano_path=${1:-../cardano-sl}

system_start=$((`date +%s` + 1))

./scripts/run.sh $cardano_path/scripts/common-functions.sh & PIDEX=$!
WALLET_TEST=1 $cardano_path/scripts/launch/demo-without-wallet-api.sh & PIDNODE=$!

wait $PIDEX
wait $PIDNODE