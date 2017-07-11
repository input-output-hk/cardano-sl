#!/usr/bin/env bash

stack exec -- cardano-node \
    --system-start 1498234259 \
    --log-config scripts/log-templates/log-config-qa.yaml \
    --logs-prefix "logs/qanet" \
    --db-path db-qanet \
    --kademlia-peer 52.57.23.114:3000 \
    --kademlia-peer 52.57.168.157:3000 \
    --kademlia-peer 34.251.36.219:3000 \
    --kademlia-peer 34.248.254.168:3000 \
    --wallet \
    --wallet-db-path wdb-qanet \
    --static-peers \
    --no-ntp \
    $@
