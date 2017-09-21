#!/usr/bin/env bash

readonly CLUSTER=testnet-0.6
readonly DOMAIN=aws.iohk.io
readonly SYSTEM_START_TIME=1504820421

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
    --system-start "${SYSTEM_START_TIME}"                   \
    --configuration-file node/configuration.mainnet.yaml    \
    --configuration-key mainnet_base                   
