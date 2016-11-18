#!/usr/bin/env bash

base=$(dirname "$0")
. "$base"/common.sh

# create logs dir actually
# TODO update wallet executable to support
#   log params and pass params as is done for node
ensure_logs

i=$1

$(find_binary cardano-wallet) submit -i $i `dht_config rand 0`\
  | tee $logs_dir/wallet$i.log
