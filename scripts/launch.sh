#!/usr/bin/env bash

base=$(dirname "$0")
source "$base"/common.sh

# Make sure we're in a tmux session
# if [[ $TERM != screen* ]]; then
    # echo "ERROR: Must have tmux started!"
    # exit 1
# fi

# If stack-work doesn't exist use function
if [[ ! -d "$base/../.stack-work" ]]; then
    stack_build
    # TODO: Maybe wanna have an option to rebuild?
fi

# Define the default amount of nodes to run
DEFAULT_NODES_N=3

# Detect node number or use default
n=$1
if [[ "$n" == "" ]]; then
  n=$DEFAULT_NODES_N
fi

# Mode is not mandatory
mode=$2

# Stats are not mandatory either
stats=$3

panesCnt=$n

if [[ "$TPS" != "" ]]; then
  panesCnt=$((n+1))
fi

# System start time in seconds (time since epoch).
# An extra second is added so that the nodes have extra time to start up
# and start processing the first slot.
system_start=$((`date +%s` + 1))

echo "Using system start time "$system_start

i=0
while [[ $i -lt $panesCnt ]]; do
  im=$((i%4))
  ir=$((i/4))

  if [[ $im == 0 ]]; then
    tmux new-window -n "demo-"`date +%H%M%S`-"$ir"
    tmux split-window -h
    tmux split-window -v
    tmux select-pane -t 0
    tmux split-window -v
  fi

  echo "Launching node $i in tab $im of window $ir"
  tmux select-pane -t $im

  if [[ "$mode" == "no_dht" ]]; then
      dht_conf='dht_config '$i' all '$n
  else
    dht_conf='dht_config rand 0'
    if [[ $i == 0 ]]; then
      dht_conf='dht_config 0'
    fi
  fi

  wallet_args=''
  if [[ $WALLET_TEST != "" ]]; then
      if (( $i == $n - 1 )); then
          wallet_args=' --wallet' # --wallet-rebuild-db'
      fi
  fi

  stake_distr=" --flat-distr \"($n, 100000)\" "
  kademlia_dump_path="kademlia$i.dump"
  static_peers=''

  if [[ $STATIC_PEERS != "" ]]; then
      static_peers=' --static-peers'
  fi

  if [[ "$CSL_PRODUCTION" != "" ]]; then
      stake_distr=""
  fi

  if [[ $i -lt $n ]]; then
    tmux send-keys "$(node_cmd $i "$dht_conf" "$stats" "$stake_distr" "$wallet_args" "$kademlia_dump_path" "$system_start") $static_peers" C-m
  else
    tmux send-keys "NODE_COUNT=$n $base/bench/runSmartGen.sh 0 -R 1 -N 2 -t $TPS -S 3 --init-money 100000 --recipients-share 0" C-m
  fi
  i=$((i+1))
done

# Experimental:
# This would hang the script and wait for Ctrl+C
# trap "$base/kill-demos.sh; exit" INT
# while true; do
#     # nothing: just wait until the user presses Ctrl+C
#     sleep 9000
# done
