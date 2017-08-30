#!/usr/bin/env bash

stack exec -- cardano-node \
    --system-start 1504019546 \
    --log-config log-config-prod.yaml \
    --logs-prefix "logs/qanet" \
    --keyfile secret.key \
    --db-path db-qanet \
    --wallet-db-path wdb-qanet \
    --topology ./run/topology.yaml \
    --tlscert ./scripts/tls-files/server.crt \
    --tlskey ./scripts/tls-files/server.key \
    --tlsca ./scripts/tls-files/ca.crt \
    $@
