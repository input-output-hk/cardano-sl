#!/usr/bin/env bash

CLUSTER=mainnet
TOPOLOGY_FILE=/tmp/topology-$CLUSTER.yaml
OUTPATH=/tmp/cardano-$CLUSTER-blockchain_$(date +%Y-%m-%d_%H-%M-%S)

RELAY=relays.cardano-mainnet.iohk.io

echo "\
wallet:
  relays: [[{ host: $RELAY }]]
  valency: 1
  fallbacks: 7
" > $TOPOLOGY_FILE

EXECMODE=""
PEERS=""

if [[ $1 == '--nowait' ]]; then
  EXECMODE="--mode=with-config cmd --commands=\"dump-blockchain $OUTPATH\""
  echo "Current state of the blockchain will be dumped."
  echo "No downloading will occur."
  echo ""
elif [[ $1 == '--wait' ]]; then
  EXECMODE="--mode=with-node repl"

  # Parse output of `nslookup` and extract IP addresses of the bootstrap nodes,
  # then format them into `--peer <IP>:3000` arguments.
  PEERS=$(\
      nslookup $RELAY | \
      awk '/^Address: / { print "--peer", $2 ":3000" }' | \
      paste -sd ' ')

  echo "Auxx does not support waiting for synchronization."
  echo "As soon as the logs show that the node is synced,"
  echo "execute the following command:"
  echo ""
  echo "    dump-blockchain $OUTPATH"
  echo ""
else
	echo "Usage: $0 [--wait|--nowait]"
  exit
fi;

EXECCMD="
stack exec -- cardano-auxx                                \
  $EXECMODE                                               \
  --no-ntp                                                \
  --topology $TOPOLOGY_FILE                               \
  $PEERS                                                  \
  --log-config ./scripts/log-templates/log-config-qa.yaml \
  --logs-prefix ./logs/$CLUSTER                           \
  --db-path ./db-$CLUSTER                                 \
  --keyfile ./secret-$CLUSTER.key                         \
  --configuration-file ./lib/configuration.yaml           \
  --configuration-key mainnet_full                        \
  --block-storage-mirror http://127.0.0.1:8080            \
"

echo $EXECCMD
eval $EXECCMD
