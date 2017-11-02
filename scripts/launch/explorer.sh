#!/usr/bin/env bash

# First argument is path to common.sh
#base=$(dirname "$0")
#. "$base"/../common-functions.sh
common_path=$1

# Check if exists, if not assign
if [ -z "$system_start" ]
  then
    system_start=$((`date +%s` + 40))
fi

# Second is the number of nodes in network (to match genesis utxo distribution)
n=3
if [[ "$2" != "" ]]; then
    n=$2
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
      --kademlia ./run/kademlia_explorer.yaml \
      --no-ntp"
echo "$cmd"
$cmd
#tmux select-pane -t 3
#tmux send-keys "$cmd" C-m

