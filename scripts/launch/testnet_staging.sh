#!/usr/bin/env bash

stack exec -- cardano-node \
    --system-start 1504019546 \
    --log-config scripts/log-templates/log-config-qa.yaml \
    --logs-prefix "logs/testnet-staging" \
    --db-path db-testnet-staging \
    --wallet-db-path wdb-testnet-staging \
    --no-ntp \
    --topology ./run/topology.yaml
    --tlscert ./scripts/tls-files/server.crt \
    --tlskey ./scripts/tls-files/server.key \
    --tlsca ./scripts/tls-files/ca.crt \
    $@
