#!/usr/bin/env bash

base=$(dirname "$0")

WALLET_EXTRA_ARGS="--new-wallet" WALLET_EXE_NAME='cardano-node' WALLET_TEST=1 "$base"/demo.sh $@
