#!/usr/bin/env bash

# Remove logs
rm *.log

# Optional path for `cardano-sl`
cardano_path=${1:-../cardano-sl}

./scripts/run.sh $cardano_path/scripts/common-functions.sh & PIDEX=$!
$cardano_path/scripts/launch/demo-with-wallet-api.sh & PIDNODE=$!

wait $PIDEX
wait $PIDNODE