#!/bin/sh

mkdir -p logs

i=$1

if [[ $i -lt 10 ]]; then
  port="300$i"
else
  port="30$i"
fi

st=$2
if [[ $st != "" ]]; then
  st=" --start-time "$st"µs"
fi

stack exec -- pos-node --db-path pos-db$i --rebuild-db --vss-genesis $i \
  --spending-genesis $i --port $port --peer '127.0.0.1:2000/ABOtPlQMv123_4wzfgjAzvsT2LE=' \
  $st \
  | tee logs/node-$i-`date '+%F_%H%M%S'`.log
