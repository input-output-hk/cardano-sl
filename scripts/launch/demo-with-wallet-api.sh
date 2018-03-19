#!/usr/bin/env bash

base=$(dirname "$0")

NO_REBUILD=1 system_start=1519749355 WALLET_TEST=1 "$base"/demo.sh $@
