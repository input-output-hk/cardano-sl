#!/usr/bin/env bash
set -eo pipefail

readonly CLUSTER=dry-run-1.0
readonly DOMAIN=awstest.iohkdev.io

if [[ "$1" == "-c" ]]; then
  shift
  rm -Rf \
    db-${CLUSTER}                                 \
    wdb-${CLUSTER}                         \
    secret-$CLUSTER.key \
    logs/$CLUSTER
fi

echo "Launch a single node and connect it to '${CLUSTER}' cluster..."

readonly TMP_TOPOLOGY_YAML=/tmp/topology.yaml
printf "wallet:
    relays:
        [
            [
                { host: cardano-node-0.${DOMAIN}, port: 3000 },
                { host: cardano-node-1.${DOMAIN}, port: 3000 },
                { host: cardano-node-2.${DOMAIN}, port: 3000 },
                { host: cardano-node-3.${DOMAIN}, port: 3000 },
                { host: cardano-node-4.${DOMAIN}, port: 3000 },
                { host: cardano-node-5.${DOMAIN}, port: 3000 },
                { host: cardano-node-6.${DOMAIN}, port: 3000 }
            ]
        ]
    valency: 1
    fallbacks: 7" > "${TMP_TOPOLOGY_YAML}"

$(nix-build -A cardano-sl-wallet)/bin/cardano-node          \
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
    --configuration-file node/configuration.yaml            \
    --configuration-key mainnet_wallet_macos64              \
    +RTS -p -h -RTS
