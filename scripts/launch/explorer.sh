#!/usr/bin/env bash

# Remove logs
rm *.log

system_start=$((`date +%s` + 1))

./explorer/scripts/run.sh ./scripts/common-functions.sh & PIDEX=$!
WALLET_TEST=1 ./scripts/launch/demo-with-wallet-api.sh & PIDNODE=$!

wait $PIDEX
wait $PIDNODE
