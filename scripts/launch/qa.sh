#!/usr/bin/env bash

stack exec -- cardano-node \
    --system-start 1497640619 \
    --kademlia-explicit-initial \
    --log-config scripts/log-templates/log-config-qa.yaml \
    --logs-prefix "logs/qanet" \
    --db-path db-qanet \
    --kademlia-peer 52.57.159.95:3000 \
    --wallet \
    --wallet-db-path wdb-qanet \
    --kademlia-dump-path kademlia-qanet.dump \
    --static-peers \
    $@
