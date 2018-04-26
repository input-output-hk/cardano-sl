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

readonly TOPOLOGY_YAML=docs/network/example-topologies/mainnet-staging.yaml

LANG=en_GB.UTF-8 LC_ALL=en_GB.UTF-8 stack exec -- cardano-node                                 \
    --tlscert ./scripts/tls-files/server.crt               \
    --tlskey ./scripts/tls-files/server.key                \
    --tlsca ./scripts/tls-files/ca.crt                     \
    --topology "${TOPOLOGY_YAML}"                          \
    --log-config log-configs/connect-to-cluster.yaml       \
    --logs-prefix "logs/${CLUSTER}"                        \
    --db-path db-${CLUSTER}                                \
    --wallet-db-path wdb-${CLUSTER}                        \
    --keyfile secret-$CLUSTER.key                          \
    --configuration-file lib/configuration.yaml            \
    --configuration-key mainnet_dryrun_full
