#!/usr/bin/env bash

tmux new-window -n "cardano-bench-test-"`date +%F_%H%M%S`

tmux split-window -h
tmux split-window -v
tmux select-pane -t 0
tmux split-window -v

startTime="$(date +%s)$(date +%N | sed 's/...$//')"

tmux select-pane -t 0
tmux send-keys "./scripts/runBenchSupporter.sh" C-m

tmux select-pane -t 1
tmux send-keys "TIME_LORD=1 ./scripts/runBenchNode.sh 0" C-m

tmux select-pane -t 2
tmux send-keys "./scripts/runBenchNode.sh 1" C-m

tmux select-pane -t 3
tmux send-keys "./scripts/runBenchNode.sh 2" C-m
