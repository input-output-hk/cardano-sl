#!/usr/bin/env bash

stack exec -- cardano-node \
    --system-start 1499802478 \
    --log-config scripts/log-templates/log-config-qa.yaml \
    --logs-prefix "logs/qanet" \
    --db-path db-qanet \
    --kademlia-peer 52.58.131.170:3000 \
    --kademlia-peer 35.158.246.75:3000 \
    --kademlia-peer 34.249.252.215:3000 \
    --kademlia-peer 52.211.65.215:3000 \
    --wallet \
    --wallet-db-path wdb-qanet \
    --static-peers \
    --no-ntp \
    $@