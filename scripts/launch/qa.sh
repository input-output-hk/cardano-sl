#!/usr/bin/env bash

stack exec -- cardano-node \
    --system-start 1501793381 \
    --log-config scripts/log-templates/log-config-qa.yaml \
    --logs-prefix "logs/qanet" \
    --db-path db-qanet \
    --kademlia-peer 52.58.155.31:3000 \
    --kademlia-peer 52.58.223.11:3000 \
    --kademlia-peer 52.48.111.191:3000 \
    --kademlia-peer 54.72.213.16:3000 \
    --wallet \
    --wallet-db-path wdb-qanet \
    --static-peers \
    --no-ntp \
    $@
