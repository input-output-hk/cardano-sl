#!/usr/bin/env bash

# First argument is path to common.sh
common_path=$1
system_start=$((`date +%s` + 1))

# Second is the number of nodes in network (to match genesis utxo distribution)
n=3
if [[ "$2" != "" ]]; then
    n=$2
fi

source "$common_path"
cmd="stack exec cardano-explorer -- `dht_config rand 0` \
      --rebuild-db \
      --flat-distr ($n,100000) \
      --listen 127.0.0.1:$((3000+$n)) \
      --system-start $system_start \
      --log-config log-config.yaml"
echo "$cmd"
$cmd
