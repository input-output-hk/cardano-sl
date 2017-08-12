#!/usr/bin/env bash

stack exec -- cardano-node \
    --system-start 1501793381 \
    --log-config scripts/log-templates/log-config-qa.yaml \
    --logs-prefix "logs/qanet" \
    --db-path db-qanet \
    --wallet \
    --wallet-db-path wdb-qanet \
    --no-ntp \
    $@
