#!/usr/bin/env bash

n=$1

filter=$2

i=0

echo 'digraph comm {'

while [[ $i -lt $n ]]; do
  i2=$i
  if [[ $i -lt 10 ]]; then
      i2="0$i"
  fi
  grep rejoinNetwork node-$i.log | tail -1 | sed -r 's/^.*peers \[//' | sed 's/\]$//' | tr "," "\n" | sed -r 's/^.* at 127.0.0.1:30([0-9]*)$/\1/' \
    | sort | uniq | while read id; do
    echo "node$i2 -> node$id;"
  done
  i=$((i+1))
done

echo '}'
