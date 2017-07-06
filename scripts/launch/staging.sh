#!/usr/bin/env bash

stack exec -- cardano-node \
    --system-start 1498743070 \
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
    --wallet \
    --wallet-db-path wdb-qanet \
    --static-peers \
    $@
