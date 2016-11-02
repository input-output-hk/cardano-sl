#!/usr/bin/env bash

base=$(dirname "$0")
. "$base"/common.sh
ensure_logs

i=$1
delay=""
if [ ! -z "$2" ]; then
    delay="-d $2"
fi

$(find_build_binary pos-tx-generator) --peer '127.0.0.1:8081/AMImyVJDsGJqqRbySRDaJ9uP5CU=' -i $i $delay \
                                      2>&1 | tee logs/tx-generator-$i-`date '+%F_%H%M%S'`.log
