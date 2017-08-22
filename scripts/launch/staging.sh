#!/usr/bin/env bash

stack exec -- cardano-node \
    --system-start 1499246772 \
    --log-config log-config-prod.yaml \
    --logs-prefix "logs/qanet" \
    --db-path db-qanet \
    --wallet-db-path wdb-qanet \
    --no-ntp \
    $@
