#!/usr/bin/env bash

stack exec -- cardano-node \
    --system-start 1499246772 \
    --log-config log-config-prod.yaml \
    --logs-prefix "logs/testnet-public" \
    --db-path db-testnet-public \
    --wallet-db-path wdb-testnet-public \
    --no-ntp \
    $@
