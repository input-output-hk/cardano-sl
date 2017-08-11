#!/usr/bin/env bash

base=$(dirname "$0")
. "$base"/../common-functions.sh

ensure_logs

if [[ "$SSC_ALGO" != "" ]]; then
    ssc_algo=" --ssc-algo $SSC_ALGO "
fi

system_start=100500 # random value, not used, but mandatory

$(find_binary cardano-wallet) $(peer_config 0) $(logs smartwallet$i.log) \
                              --db-path "run/wallet-db" --rebuild-db \
                              --flat-distr "(3, 100000)" $ssc_algo "$@" \
                              --system-start $system_start \
                              repl
