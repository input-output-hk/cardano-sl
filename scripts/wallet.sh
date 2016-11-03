#!/usr/bin/env bash

base=$(dirname "$0")
. "$base"/common.sh
ensure_logs

i=$1

$(find_binary cardano-wallet) submit -i $i `dht_config rand 0`\
  | tee $logs_dir/wallet-$i-`date '+%F_%H%M%S'`.log
