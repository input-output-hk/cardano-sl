#!/usr/bin/env bash

tmux list-windows | \grep demo | awk -F ':' '{ print $1 }' | while read i
do
  tmux kill-window -t "$i"
done
