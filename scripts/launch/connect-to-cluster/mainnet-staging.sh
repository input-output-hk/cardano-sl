#!/usr/bin/env bash
set -eo pipefail

readonly CLUSTER=mainnet-staging

if [[ "$1" == "-c" ]]; then
  shift
  rm -Rf                \
    db-${CLUSTER}       \
    wdb-${CLUSTER}      \
    secret-$CLUSTER.key \
    logs/$CLUSTER
fi

echo "Launch a single node and connect it to '${CLUSTER}' cluster..."

readonly TMP_TOPOLOGY_YAML=/tmp/topology.yaml
printf "wallet:
    relays: [[{ host: relays.awstest.iohkdev.io }]]
    valency: 1
    fallbacks: 7" > "${TMP_TOPOLOGY_YAML}"

stack exec -- cardano-node                                  \
    --tlscert ./scripts/tls-files/server.crt                \
    --tlskey ./scripts/tls-files/server.key                 \
    --tlsca ./scripts/tls-files/ca.crt                      \
    --web                                                   \
    --no-ntp                                                \
    --topology "${TMP_TOPOLOGY_YAML}"                       \
    --log-config scripts/log-templates/log-config-qa.yaml   \
    --logs-prefix "logs/${CLUSTER}"                         \
    --db-path db-${CLUSTER}                                 \
    --wallet-db-path wdb-${CLUSTER}                         \
    --keyfile secret-$CLUSTER.key                           \
    --configuration-file lib/configuration.yaml             \
    --configuration-key mainnet_dryrun_full
