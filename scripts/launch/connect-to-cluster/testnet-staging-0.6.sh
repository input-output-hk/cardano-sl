#!/usr/bin/env bash
set -euo pipefail

echo "Launch a single node and connect it to 'testnet-staging-0.6' cluster..."

readonly TMP_TOPOLOGY_YAML=/tmp/topology.yaml
readonly SYSTEM_START_TIME=1503504180

printf "wallet:
    relays:
        [
            [
                { host: cardano-node-0.aws.iohkdev.io, port: 3000 },
                { host: cardano-node-1.aws.iohkdev.io, port: 3000 },
                { host: cardano-node-2.aws.iohkdev.io, port: 3000 },
                { host: cardano-node-3.aws.iohkdev.io, port: 3000 },
                { host: cardano-node-4.aws.iohkdev.io, port: 3000 },
                { host: cardano-node-5.aws.iohkdev.io, port: 3000 },
                { host: cardano-node-6.aws.iohkdev.io, port: 3000 }
            ]
        ]
    valency: 3
    fallbacks: 2" > "${TMP_TOPOLOGY_YAML}"

stack exec -- cardano-node                                  \
    --tlscert ./scripts/tls-files/server.crt                \
    --tlskey ./scripts/tls-files/server.key                 \
    --tlsca ./scripts/tls-files/ca.crt                      \
    --no-ntp                                                \
    --topology "${TMP_TOPOLOGY_YAML}"                       \
    --log-config scripts/log-templates/log-config-qa.yaml   \
    --logs-prefix "logs/testnet-staging-0.6"                \
    --db-path db-testnet-staging-0.6                        \
    --wallet-db-path wdb-testnet-staging-0.6                \
    --system-start "${SYSTEM_START_TIME}"
