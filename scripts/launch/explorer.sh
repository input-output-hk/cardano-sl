#!/usr/bin/env bash

# First argument is path to `common-functions.sh`
common_path=$1

# Assign `$common_path` if not set before
if [ -z "$common_path" ]
  then
    base=$(dirname "$0")
    common_path="$base"/../common-functions.sh
fi

# Check if `$system_start` exists, if not assign
if [ -z "$system_start" ]
  then
    system_start=$((`date +%s))
fi

# Second argument is the number of nodes in network
# (to match genesis utxo distribution)
n=4
if [[ "$2" != "" ]]; then
    n=$2
fi

# We presume we already launched n nodes before, we select the last pane.
# TODO: I'm not sure that choosing topology0.yaml is a correct way to do (@volhovm)
source "$common_path"
cmd="stack exec cardano-explorer --
      --rebuild-db \
      --flat-distr ($n,100000) \
      --listen 127.0.0.1:300$n \
      --system-start $system_start \
      --log-config explorer/log-config.yaml \
      --topology ./run/topology0.yaml \
      --node-id explorer \
      --kademlia ./run/kademlia_explorer.yaml \
      --no-ntp"
echo "$cmd"
$cmd
#tmux select-pane -t 3
#tmux send-keys "$cmd" C-m
