#!/usr/bin/env bash
set -eo pipefail

readonly CLUSTER=mainnet

echo "Launch a single explorer node and connect it to '${CLUSTER}' cluster..."

readonly TMP_TOPOLOGY_YAML=/tmp/topology.yaml
printf "wallet:
    relays: [[{ host: relays.cardano-mainnet.iohk.io }]]
    valency: 1
    fallbacks: 7" > "${TMP_TOPOLOGY_YAML}"

stack exec --  cardano-explorer                             \
    --no-ntp                                                \
    --topology "${TMP_TOPOLOGY_YAML}"                       \
    --log-config scripts/log-templates/log-config-qa.yaml   \
    --logs-prefix "logs/${CLUSTER}"                         \
    --keyfile secret-$CLUSTER.key                           \
    --configuration-file node/configuration.yaml            \
    --configuration-key mainnet_wallet_macos64

# cmd="stack exec--
#       --rebuild-db \
#       --flat-distr ($n,100000) \
#       --listen 127.0.0.1:300$n \
#       --system-start $system_start \
#       --log-config explorer/log-config.yaml \
#       --topology ./run/topology0.yaml \
#       --kademlia ./run/kademlia_explorer.yaml \
#       --no-ntp"
# echo "$cmd"