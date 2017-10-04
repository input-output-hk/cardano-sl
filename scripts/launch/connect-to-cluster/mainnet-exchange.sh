#!/usr/bin/env bash
set -eo pipefail

readonly CLUSTER=mainnet

flush_wallet=""

if [[ "$1" == "-l" ]]; then
  shift
  flush_wallet="--flush-wallet-db"
fi

echo "Launch a single node and connect it to '${CLUSTER}' cluster..."

readonly TMP_TOPOLOGY_YAML=/tmp/topology.yaml
printf 'wallet:
 relays: [ [ {"host": "cardano-node-0.cardano-mainnet.iohk.io"}
           , {"host": "cardano-node-1.cardano-mainnet.iohk.io"}
           , {"host": "cardano-node-6.cardano-mainnet.iohk.io"}
           ]
         , [ {"host": "cardano-node-2.cardano-mainnet.iohk.io"}
           , {"host": "cardano-node-3.cardano-mainnet.iohk.io"}
           , {"host": "cardano-node-6.cardano-mainnet.iohk.io"}
           ]
         , [ {"host": "cardano-node-4.cardano-mainnet.iohk.io"}
           , {"host": "cardano-node-5.cardano-mainnet.iohk.io"}
           , {"host": "cardano-node-6.cardano-mainnet.iohk.io"}
           ]
         ]' > "${TMP_TOPOLOGY_YAML}"

stack exec -- cardano-node                                  \
    --tlscert ./scripts/tls-files/server.crt                \
    --tlskey ./scripts/tls-files/server.key                 \
    --tlsca ./scripts/tls-files/ca.crt                      \
    --no-ntp                                                \
    --topology "${TMP_TOPOLOGY_YAML}"                       \
    --log-config scripts/log-templates/log-config-qa.yaml   \
    --logs-prefix "logs/${CLUSTER}"                         \
    --db-path db-${CLUSTER}                                 \
    --wallet-db-path wdb-${CLUSTER}                         \
    --keyfile secret-$CLUSTER.key                           \
    --wallet-acid-cleanup-interval=180                      \
    --configuration-file node/configuration.yaml    \
    --configuration-key mainnet_full $flush_wallet
