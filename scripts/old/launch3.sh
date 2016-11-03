#!/bin/sh

base=$(dirname "$0")

tmux new-window -n "cardano-demo-"`date +%F_%H%M%S`

tmux split-window -h
tmux split-window -v
tmux select-pane -t 0
tmux split-window -v

envs="MAIN_LOG=$MAIN_LOG DHT_LOG=$DHT_LOG COMM_LOG=$COMM_LOG SERVER_LOG=$SERVER_LOG"

tmux select-pane -t 0
tmux send-keys "$envs $base/runSupporter.sh" C-m

tmux select-pane -t 1
tmux send-keys "$envs TIME_LORD=1 $base/runNode.sh 0" C-m

tmux select-pane -t 2
tmux send-keys "$envs $base/runNode.sh 1" C-m

tmux select-pane -t 3
tmux send-keys "$envs $base/runNode.sh 2" C-m
