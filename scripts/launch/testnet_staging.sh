#!/usr/bin/env bash

stack exec -- cardano-node \
    --system-start 1501793381 \
    --log-config scripts/log-templates/log-config-qa.yaml \
    --logs-prefix "logs/testnet-staging" \
    --db-path db-testnet-staging \
    --wallet-db-path wdb-testnet-staging \
    --no-ntp \
    $@
