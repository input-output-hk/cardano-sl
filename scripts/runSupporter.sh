#!/usr/bin/env bash

base=$(dirname "$0")
. "$base"/common.sh
ensure_logs


$(find_binary cardano-node) --supporter --port 2000 --dht-key 'ABOtPlQMv123_4wzfgjAzvsT2LE='\
  $logs | tee logs/supporter-`date '+%F_%H%M%S'`.log
