#!/usr/bin/env bash

for f in node*.log; do
  grep -E 'New slot|I am leader|Created a new block|Received block has been adopted|chain has been adopted|considered useful' $f \
    | sed -r 's/^(\S*)\s(.*)$/\2 \1/' | while read l; do
      echo "$l [$f]"
    done
  done|sort
