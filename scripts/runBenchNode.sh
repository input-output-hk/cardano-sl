#!/usr/bin/env bash

base=$(dirname "$0")
. "$base"/common.sh
ensure_logs

i=$1
config="$base/../bench/config/fullnode.$i.yaml"

st=""
if [[ $TIME_LORD != "" ]]; then
  st="--time-lord"
fi

$(find_build_binary pos-bench-remote-single) full -n $i $st --config $config \
                                             2>&1 | tee logs/node-bench-$i-`date '+%F_%H%M%S'`.log
