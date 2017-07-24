#!/usr/bin/env bash

stack exec -- cardano-node \
    --system-start 1499360281 \
    --log-config log-config-prod.yaml \
    --logs-prefix "Logs" \
    --db-path "DB-0.5" \
    --kademlia-peer cardano-node-0.aws.iohk.io:3000 \
    --kademlia-peer cardano-node-2.aws.iohk.io:3000 \
    --kademlia-peer cardano-node-6.aws.iohk.io:3000 \
    --kademlia-peer cardano-node-10.aws.iohk.io:3000 \
    --wallet \
    --wallet-db-path "Wallet-0.5" \
    --static-peers \
    $@