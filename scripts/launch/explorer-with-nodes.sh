#!/usr/bin/env bash

system_start=$((`date +%s` + 15))

WALLET_TEST=1 system_start=$system_start ./scripts/launch/demo-with-wallet-api.sh & PIDNODE=$!
system_start=$system_start ./scripts/launch/explorer.sh & PIDEX=$!

wait $PIDEX
wait $PIDNODE
