#!/usr/bin/env bash
set -o xtrace

# Make sure we're in a tmux session.
if ! [ -n "$TMUX" ]; then
  echo "You must run this script from the tmux session!"
  exit 1
fi

base=$(dirname "$0")
source "$base"/../common-functions.sh

# If stack-work doesn't exist use function
if [[ ! -d "$base/../../.stack-work" ]]; then
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

# CORE_NODES specifies how many nodes should be core nodes, i.e., have non-negligible stake in the rich_poor_distr
if [[ "$CORE_NODES" == "" ]]; then
  CORE_NODES == 3
fi

config_dir=$2

if [[ $config_dir == "" ]]
  then
    config_dir="./run"
fi

# The stake distribution.
# Use "flat" for flat_distr. Anything else will use rich_poor_distr
stake_distr_param=$3
flat_distr=" --flat-distr \"($n, 100000)\" "
rich_poor_distr=" --rich-poor-distr \"($CORE_NODES,50000,6000000000,0.99)\" "

# Stats are not mandatory either
stats=$4

panesCnt=$n

if [[ "$CONC" != "" ]]; then
  panesCnt=$((n+1))
fi

if [[ "$NUM_TXS" == "" ]]; then
  NUM_TXS=3000
fi

# System start time in seconds (time since epoch).
# An extra second is added so that the nodes have extra time to start up
# and start processing the first slot.
if [ -z "$system_start" ]
  then
    system_start=$((`date +%s` + 30))
fi

echo "Using system start time "$system_start

echo "Number of panes: $panesCnt"

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

  wallet_args=''
  exec_name='cardano-node-simple'
  if [[ $WALLET_TEST != "" ]]; then
      if (( $i == $n - 1 )); then
          wallet_args=" --tlscert $base/../tls-files/server.crt --tlskey $base/../tls-files/server.key --tlsca $base/../tls-files/ca.crt " # --wallet-rebuild-db'
          exec_name='cardano-node'
          if [[ $WALLET_DEBUG != "" ]]; then
              wallet_args="$wallet_args --wallet-debug"
          fi
      fi
  fi

  if [[ $stake_distr_param == "rich_poor" ]]; then
    stake_distr=$rich_poor_distr
  else
    stake_distr=$flat_distr
  fi

  if [[ "$CSL_PRODUCTION" != "" ]]; then
      stake_distr=""
  fi

  if [[ $i -lt $n ]]; then
    tmux send-keys "$(node_cmd $i "$stats" "$stake_distr" "$wallet_args" "$system_start" "$config_dir" $exec_name) --no-ntp" C-m
  else
    # Number of transactions to send per-thread: 300
    # Concurrency (number of threads sending transactions); $CONC
    # Delay between sends on each thread: 500 milliseconds
    tmux send-keys "sleep 40s && $(bench_cmd $i "$stake_distr" "$system_start" $NUM_TXS $CONC 500 neighbours)" C-m
  fi
  i=$((i+1))
done

# Experimental:
# This would hang the script and wait for Ctrl+C
# trap "$base/kill-demo.sh; exit" INT
# while true; do
#     # nothing: just wait until the user presses Ctrl+C
#     sleep 9000
# done
