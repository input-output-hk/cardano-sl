#!/usr/bin/env bash

base=$(dirname "$0")
. "$base"/common.sh
ensure_logs

config="$base/../bench/config/supporter.yaml"

$(find_build_binary pos-bench-remote-single) supporter --config $config \
                                             2>&1 | tee logs/supporter-bench-$i-`date '+%F_%H%M%S'`.log
