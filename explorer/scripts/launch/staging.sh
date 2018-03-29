#!/usr/bin/env bash

# clear old data, don't remove the databases since they may contains some data
# that will help speed up the syncing process
rm -rf run/* node-* *key* *.dump

stack exec -- cardano-explorer \
    --system-start 1499246772 \
    --log-config log-config-prod.yaml \
    --logs-prefix "logs/qanet" \
    --db-path db-qanet \
    --kademlia-peer cardano-node-0.aws.iohkdev.io:3000 \
    --kademlia-peer cardano-node-1.aws.iohkdev.io:3000 \
    --kademlia-peer cardano-node-2.aws.iohkdev.io:3000 \
    --kademlia-peer cardano-node-3.aws.iohkdev.io:3000 \
    --kademlia-peer cardano-node-4.aws.iohkdev.io:3000 \
    --kademlia-peer cardano-node-5.aws.iohkdev.io:3000 \
    --kademlia-peer cardano-node-6.aws.iohkdev.io:3000 \
    --kademlia-peer cardano-node-7.aws.iohkdev.io:3000 \
    --kademlia-peer cardano-node-8.aws.iohkdev.io:3000 \
    --kademlia-peer cardano-node-9.aws.iohkdev.io:3000 \
    --kademlia-peer cardano-node-10.aws.iohkdev.io:3000 \
    --kademlia-peer cardano-node-11.aws.iohkdev.io:3000 \
    --kademlia-peer cardano-node-12.aws.iohkdev.io:3000 \
    --kademlia-peer cardano-node-13.aws.iohkdev.io:3000 \
    --listen 127.0.0.1:$((3000)) \
    --static-peers \
    $@