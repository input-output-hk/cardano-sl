#!/usr/bin/env bash

base=$(dirname "$0")
. "$base"/../common.sh

# create logs dir actually
# TODO update wallet executable to support
#   log params and pass params as is done for node
ensure_logs

i=$1
shift

if [[ "$SSC_ALGO" != "" ]]; then
    ssc_algo=" --ssc-algo $SSC_ALGO "
fi

$(find_binary cardano-smart-generator) $(peer_config $i) $(logs smartgen$i.log) \
                                       --flat-distr "(3, 100000)" $ssc_algo -i $i $@
