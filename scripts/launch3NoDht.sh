#!/bin/sh

base=$(dirname "$0")
. "$base"/common.sh
ensure_logs

function get_port {
  i=$1
  if [[ $i -lt 10 ]]; then
    i2="0$i"
  else
    i2=$i
  fi
  echo "30$i2"
}
function dht_key {
  i=$1
  if [[ $i -lt 10 ]]; then
    i2="0$i"
  else
    i2=$i
  fi
  echo "MHdtsP-oPf7UWly"$i2"7QuXnLK5RD="
}

function node_cmd {
  i=$1
  n=$2
  TIME_LORD=$3


  echo -n "$(find_binary pos-node) --db-path pos-db$i --rebuild-db --vss-genesis $i"

  if [[ $TIME_LORD != "" ]]; then
    st=" --time-lord"
  fi

  j=0
  while [[ $j -lt $n ]]; do
      echo -n " --peer 127.0.0.1:"`get_port $j`'/'`dht_key $j`
      j=$((j+1))
  done
  echo -n " --spending-genesis $i --port "`get_port $i`
  echo -n " --dht-key "`dht_key $i`" $logs $st --explicit-initial | tee logs/node-$i-"`date '+%F_%H%M%S'`".log "
  echo ''
}

tmux new-window -n "pos-demo-"`date +%F_%H%M%S`

tmux split-window -h
tmux split-window -v
tmux select-pane -t 0
tmux split-window -v

startTime="$(date +%s)$(date +%N | sed 's/...$//')"

tmux select-pane -t 0

tmux select-pane -t 1
tmux send-keys "$(node_cmd 0 3 true)" C-m

tmux select-pane -t 2
tmux send-keys "$(node_cmd 1 3 true)" C-m

tmux select-pane -t 3
tmux send-keys "$(node_cmd 2 3 true)" C-m
