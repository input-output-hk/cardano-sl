#!/usr/bin/env bash

base=$(dirname "$0")
. "$base"/common.sh

ensure_logs

i=$1
shift

if [[ "$SSC_ALGO" != "" ]]; then
    ssc_algo=" --ssc-algo $SSC_ALGO "
fi

$(find_binary cardano-smart-wallet) $(peer_config $i) $(logs smartwallet$i.log) \
                                    --db-path "run/wallet-db" \
                                    --flat-distr "(3, 100000)" $ssc_algo -i $i $@
