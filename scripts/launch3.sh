#!/bin/bash

tmux new-window -n "pos-demo-"`date +%F_%H%M%S`

tmux split-window -h
tmux split-window -v
tmux select-pane -t 0
tmux split-window -v

startTime=`date +%s`$((`date +%N`/1000))

tmux select-pane -t 0
tmux send-keys "./scripts/runSupporter.sh" C-m

tmux select-pane -t 1
tmux send-keys "./scripts/runNode.sh 0 $startTime" C-m

tmux select-pane -t 2
tmux send-keys "./scripts/runNode.sh 1 $startTime" C-m

tmux select-pane -t 3
tmux send-keys "./scripts/runNode.sh 2 $startTime" C-m
