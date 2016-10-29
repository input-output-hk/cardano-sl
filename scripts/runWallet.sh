#!/bin/bash

base=$(dirname "$0")
. "$base"/common.sh
ensure_logs

i=$1

$(find_binary pos-wallet) submit -i $i --peer '127.0.0.1:2000/ABOtPlQMv123_4wzfgjAzvsT2LE='\
  | tee logs/wallet-$i-`date '+%F_%H%M%S'`.log
