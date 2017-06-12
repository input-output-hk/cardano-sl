#!/usr/bin/env bash

# Remove logs
rm *.log

# Optional path for `cardano-sl`
cardano_path=${1:-../cardano-sl}

./scripts/run.sh $cardano_path/scripts/common.sh & PIDEX=$!
$cardano_path/util-scripts/start-prod.sh & PIDNODE=$!
wait $PIDEX
wait $PIDNODE
