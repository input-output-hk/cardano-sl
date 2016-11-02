#!/usr/bin/env bash

tmux new-window -n "cardano-bench-test-"`date +%F_%H%M%S`

tmux split-window -h
tmux split-window -v
tmux select-pane -t 0

startTime="$(date +%s)$(date +%N | sed 's/...$//')"

tmux select-pane -t 0
tmux send-keys "./scripts/runTxGen.sh 0 $1" C-m

tmux select-pane -t 1
tmux send-keys "./scripts/runTxGen.sh 1 $1" C-m

tmux select-pane -t 2
tmux send-keys "./scripts/runTxGen.sh 2 $1" C-m
