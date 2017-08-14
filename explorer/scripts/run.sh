#!/usr/bin/env bash

# First argument is path to common.sh
common_path=$1

# Check if exists, if not assign
if [ -z "$system_start" ]
  then
    system_start=$((`date +%s` + 1))
fi

# Second is the number of nodes in network (to match genesis utxo distribution)
n=3
if [[ "$2" != "" ]]; then
    n=$2
fi

source "$common_path"
cmd="stack exec cardano-explorer \
      --rebuild-db \
      --flat-distr ($n,100000) \
      --listen 127.0.0.1:$((3000+$n)) \
      --system-start $system_start \
      --kademlia-peer localhost:3000 \
      --kademlia-peer localhost:3001 \
      --kademlia-peer localhost:3002 \
      --static-peers \
      --log-config log-config.yaml"

echo "$cmd"
$cmd
