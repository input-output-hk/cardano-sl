#!/bin/sh

mkdir -p logs

stack exec -- pos-node --supporter --port 2000 --dht-key 'ABOtPlQMv123_4wzfgjAzvsT2LE='\
  | tee logs/supporter-`date '+%F_%H%M%S'`.log
