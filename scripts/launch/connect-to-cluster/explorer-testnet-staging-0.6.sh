#!/usr/bin/env bash
set -euo pipefail

readonly CLUSTER=testnet-staging-0.6
readonly DOMAIN=awstest.iohkdev.io
readonly SYSTEM_START_TIME=1505689646

# readonly LOG_CONFIG=/nix/store/wh6z2ksmb042vc5r8ar9m75ry7b2lcfy-csl-logging.yaml
readonly LOG_CONFIG=scripts/log-templates/log-config-qa.yaml

# readonly LOG_PREFIX=/var/lib/cardano-node
readonly LOG_PREFIX="logs/${CLUSTER}"

echo "Launch a single node and connect it to '${CLUSTER}' cluster..."

# readonly TMP_TOPOLOGY_YAML=/nix/store/v5cz2w285ca5cpsbx1fq10cgmm3vzlym-topology-explorer.yaml
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
    valency: 3
    fallbacks: 2" > "${TMP_TOPOLOGY_YAML}"

stack exec -- cardano-explorer                                                     \
    --no-ntp                                                                       \
    --system-start 1505689646                                                      \
    --db-path db-${CLUSTER}                                                        \
    --topology "${TMP_TOPOLOGY_YAML}"                                              \
    --node-id explorer                                                             \
    --custom-config-name testnet_staging_full

    # --db-path /var/lib/cardano-node//node-db
    # --topology /nix/store/v5cz2w285ca5cpsbx1fq10cgmm3vzlym-topology-explorer.yaml
    # --node-id explorer
    # --custom-config-name testnet_staging_full

    # --tlscert ./scripts/tls-files/server.crt                \
    # --tlskey ./scripts/tls-files/server.key                 \
    # --tlsca ./scripts/tls-files/ca.crt                      \
    # --no-ntp                                                \
    # --topology "${TMP_TOPOLOGY_YAML}"                       \
    # --log-config scripts/log-templates/log-config-qa.yaml   \
    # --logs-prefix "logs/${CLUSTER}"                         \
    # --db-path db-${CLUSTER}                                 \
    # --wallet-db-path wdb-${CLUSTER}                         \
    # --system-start "${SYSTEM_START_TIME}"


