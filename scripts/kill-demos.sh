#!/usr/bin/env bash

tmux list-windows | grep demo | sed -r 's/^([0-9]+):.*$/\1/' | while read i; do tmux kill-window -t "$i"; done
