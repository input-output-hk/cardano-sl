#!/usr/bin/env bash

# First argument is path to common.sh
if [ -z "$1" ]
  then
    base=$(dirname "$0")
    common_path="$base"/../common-functions.sh
fi

# Second is the number of nodes in network (to match genesis utxo distribution)
if [[ "$2" != "" ]]; then
    n=$2
else
    n=4
fi

# Check if exists, if not assign
if [ -z "$system_start" ]
  then
    system_start=$((`date +%s` + 45))
fi

# We presume we already launched n nodes before, we select the last pane.
# TODO: I'm not sure that choosing topology0.yaml is a correct way to do (@volhovm)
source "$common_path"
cmd="stack exec cardano-explorer --
      --rebuild-db \
      --listen 127.0.0.1:300$n \
      --system-start $system_start \
      --log-config explorer/log-config.yaml \
      --topology ./run/topology0.yaml \
      --node-id explorer"
#     --kademlia ./run/kademlia_explorer.yaml \ # topology is not kademlia-suitable
echo "$cmd"
$cmd
#tmux select-pane -t 3
#tmux send-keys "$cmd" C-m
