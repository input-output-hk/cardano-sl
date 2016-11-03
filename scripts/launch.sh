#!/bin/sh

base=$(dirname "$0")
. "$base"/common.sh
ensure_logs

n=$1
if [[ "$n" == "" ]]; then
  n=3
fi
i=0

mode=$2

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
    tmux send-keys "$(node_cmd $i "$time_lord" "$dht_conf")" C-m
    i=$((i+1))
done
