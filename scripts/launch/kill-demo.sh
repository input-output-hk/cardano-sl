#!/usr/bin/env bash

# TODO (akegalj): use `tmux kill-session -t seassion-name` instead

# Make sure we're in a tmux session.
if ! [ -n "$TMUX" ]; then
  echo "There's no tmux session, so you cannot kill demo."
  exit 1
fi

tmux list-windows | \grep demo | awk -F ':' '{ print $1 }' | while read i
do
  tmux kill-window -t "$i"
done
