#!/usr/bin/env bash

base=$(dirname "$0")
source "$base"/common.sh

# Make sure we're in a tmux session
# if [[ $TERM != screen* ]]; then
    # echo "ERROR: Must have tmux started!"
    # exit 1
# fi

# If stack-work doesn't exist use function
if [[ ! -d .stack-work ]]; then
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

# Make sure the logs folder exists
ensure_logs

# Mode is not mandatory
mode=$2

# Stats are not mandatory either
stats=$3

i=0
while [[ $i -lt $n ]]; do
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

  time_lord=''
  if [[ $i == 0 ]]; then
      time_lord='time-lord'
  fi

  if [[ "$mode" == "no_dht" ]]; then
      dht_conf='dht_config '$i' all '$n
  else
    dht_conf='dht_config rand 0'
    if [[ $i == 0 ]]; then
      dht_conf='dht_config 0'
    fi
  fi

  stake_distr=" --flat-distr \"($n, 100000)\" "

  tmux send-keys "$(node_cmd $i "$time_lord" "$dht_conf" "$stats" "$stake_distr")" C-m
  i=$((i+1))
done

# Experimental:
# This would hang the script and wait for Ctrl+C
# trap "$base/kill-demos.sh; exit" INT
# while true; do
#     # nothing: just wait until the user presses Ctrl+C
#     sleep 9000
# done
