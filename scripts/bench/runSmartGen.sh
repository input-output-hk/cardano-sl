#!/usr/bin/env bash

base=$(dirname "$0")
. "$base"/../common.sh

# create logs dir actually
# TODO update wallet executable to support
#   log params and pass params as is done for node
ensure_logs

i=$1
shift

$(find_binary cardano-smart-generator) $(peer_config $i) -i $i $@ \
                                       2>&1 | tee $logs_dir/txgen$i.log
