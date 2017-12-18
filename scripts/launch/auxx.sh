#!/usr/bin/env bash

# This helper script facilitates work with cardano-auxx.
# If you run 'demo.sh' (or demo with wallet), you can this script.
# Usage: scripts/launch/auxx.sh [COMMAND] args
#
# COMMAND can be:
# 'init-dev' – initialize auxx for dev mode.
# If COMMAND is not passed, 'args' are passed directly to cardano-auxx.

base=$(dirname "$0")
. "$base"/../common-functions.sh

ensure_logs

dht_conf=" --peer 127.0.0.1:3000 --peer 127.0.0.1:3001 --peer 127.0.0.1:3002 --peer 127.0.0.1:3003 "
logs_conf=$(logs auxx.log)
binary=$(find_binary cardano-auxx)
template="$binary $dht_conf $logs_conf \
                 --system-start 100500"  # random value, not used, but mandatory

if [[ $1 == "init-dev" ]]; then
    commands="add-key-pool 0 1 2 3"
fi

if [[ "$commands" != "" ]]; then
    suffix=" cmd --commands=\"$commands\""
else
    suffix=$@
fi

to_exec="$template $suffix"

echo "$to_exec"

eval $to_exec
