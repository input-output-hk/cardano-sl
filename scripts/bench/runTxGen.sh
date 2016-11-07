#!/usr/bin/env bash

base=$(dirname "$0")
. "$base"/../common.sh
ensure_logs

i=$1
shift

$(find_binary cardano-tx-generator) $(peer_config $i) -i $i $@ \
                                    2>&1 | tee logs/tx-generator-$i-`date '+%F_%H%M%S'`.log
