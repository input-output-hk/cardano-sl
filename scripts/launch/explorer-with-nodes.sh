#!/usr/bin/env bash

system_start=$((`date +%s` + 1))

WALLET_TEST=1 ./scripts/launch/demo-with-wallet-api.sh & PIDNODE=$!
./scripts/launch/explorer.sh ./scripts/common-functions.sh & PIDEX=$!

wait $PIDEX
wait $PIDNODE
