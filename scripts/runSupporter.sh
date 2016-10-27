#!/bin/sh

mkdir -p logs

logs=''
if [[ "$DHT_LOG" != "" ]]; then
  logs="$logs --dht-log $DHT_LOG"
fi
if [[ "$MAIN_LOG" != "" ]]; then
  logs="$logs --main-log $MAIN_LOG"
fi

stack exec -- pos-node --supporter --port 2000 --dht-key 'ABOtPlQMv123_4wzfgjAzvsT2LE='\
  $logs | tee logs/supporter-`date '+%F_%H%M%S'`.log
