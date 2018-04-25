#!/usr/bin/env bash

# clear old data, don't remove the databases since they may contains some data
# that will help speed up the syncing process
rm -rf run/* node-* *key* *.dump

stack exec -- cardano-explorer \
    --system-start 1501793381 \
    --log-config log-config.yaml \
    --logs-prefix "logs/testnet" \
    --db-path db-testnet \
    --kademlia-peer 52.58.131.170:3000 \
    --kademlia-peer 35.158.246.75:3000 \
    --kademlia-peer 34.249.252.215:3000 \
    --kademlia-peer 52.211.65.215:3000 \
    --listen 127.0.0.1:$((3000)) \
    --static-peers \
    $@