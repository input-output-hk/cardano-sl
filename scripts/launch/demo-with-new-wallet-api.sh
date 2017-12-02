#!/usr/bin/env bash

base=$(dirname "$0")

WALLET_EXE_NAME='wallet-new-server' WALLET_TEST=1 "$base"/demo.sh $@
