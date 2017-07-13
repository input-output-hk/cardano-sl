#!/usr/bin/env bash

base=$(dirname "$0")
. "$base"/../common-functions.sh

ensure_logs

if [[ "$SSC_ALGO" != "" ]]; then
    ssc_algo=" --ssc-algo $SSC_ALGO "
fi

$(find_binary cardano-wallet) --system-start $(cat "$base/../../run/system_start") \
                              --log-config log-config-prod.yaml --logs-prefix "run/wallet-logs" \
                              --db-path "run/wallet-db" --rebuild-db \
                              --flat-distr "(4, 100000)" $ssc_algo "$@" \
                              --peer 127.0.0.1:3000 \
                              --peer 127.0.0.1:3001 \
                              --peer 127.0.0.1:3002 \
                              repl
