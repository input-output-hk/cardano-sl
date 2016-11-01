#!/usr/bin/env bash

base=$(dirname "$0")
. "$base"/common.sh
ensure_logs

$(find_build_binary pos-tx-generator) $@
