#!/usr/bin/env bash

base=$(dirname "$0")
. "$base"/common.sh
ensure_logs

i=$1
i2=$((i*2))

if [[ $i2 -lt 10 ]]; then
  port="300$i2"
else
  port="30$i2"
fi

if [[ $TIME_LORD != "" ]]; then
  st=" --time-lord"
fi

if [[ $NO_REBUILD == "" ]]; then
  reb=" --rebuild-db "
fi

$(find_binary pos-node) --db-path pos-db$i $reb --vss-genesis $i \
  --spending-genesis $i --port $port --peer '127.0.0.1:2000/ABOtPlQMv123_4wzfgjAzvsT2LE=' \
  $logs $st \
  2>&1 | tee logs/node-$i-`date '+%F_%H%M%S'`.log
