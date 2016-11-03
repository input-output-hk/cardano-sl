#!/usr/bin/env bash

base=$(dirname "$0")

tmux new-window -n "cardano-bench-test-"`date +%F_%H%M%S`

tmux split-window -h
tmux split-window -v
tmux select-pane -t 0
tmux split-window -v

startTime="$(date +%s)$(date +%N | sed 's/...$//')"

tmux select-pane -t 0
tmux send-keys "$base/runBenchSupporter.sh" C-m

tmux select-pane -t 1
tmux send-keys "TIME_LORD=1 $base/runBenchNode.sh 0" C-m

tmux select-pane -t 2
tmux send-keys "$base/runBenchNode.sh 1" C-m

tmux select-pane -t 3
tmux send-keys "$base/runBenchNode.sh 2" C-m
