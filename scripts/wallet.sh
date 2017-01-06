#!/usr/bin/env bash

base=$(dirname "$0")
. "$base"/common.sh

ensure_logs

if [[ "$SSC_ALGO" != "" ]]; then
    ssc_algo=" --ssc-algo $SSC_ALGO "
fi

$(find_binary cardano-wallet) $(peer_config 0) $(logs smartwallet$i.log) \
                              --db-path "run/wallet-db" --rebuild-db \
                              --flat-distr "(3, 100000)" $ssc_algo "$@"
