#!/bin/bash

base=$(dirname "$0")
. "$base"/common.sh
ensure_logs

i=$1

if [[ $i -lt 10 ]]; then
  port="300$i"
else
  port="30$i"
fi

if [[ $TIME_LORD != "" ]]; then
  st=" --time-lord"
fi


$(find_binary pos-node) --db-path pos-db$i --rebuild-db --vss-genesis $i \
  --spending-genesis $i --port $port --peer '127.0.0.1:2000/ABOtPlQMv123_4wzfgjAzvsT2LE=' \
  $logs $st \
  2>&1 | tee logs/node-$i-`date '+%F_%H%M%S'`.log
