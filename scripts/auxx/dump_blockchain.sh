#!/usr/bin/env bash

CLUSTER=mainnet
TOPOLOGY_FILE=/tmp/topology-$CLUSTER.yaml
OUTPATH=/tmp/cardano-$CLUSTER-blockchain_$(date +%Y-%m-%d_%H-%M-%S)

RELAY=relays.cardano-mainnet.iohk.io

# Parse output of `nslookup` and extract IP addresses of the bootstrap nodes,
# then format them into `--peer <IP>:3000` arguments.
PEERS=$(\
    nslookup $RELAY | \
    awk '/^Address: / { print "--peer", $2 ":3000" }' | \
    paste -sd ' ')

echo "\
wallet:
  relays: [[{ host: $RELAY }]]
  valency: 1
  fallbacks: 7
" > $TOPOLOGY_FILE

EXECMODE=""

if [[ $1 == '--nowait' ]]; then
	EXECMODE="cmd --commands=\"dump $OUTPATH\""
	echo "Auxx does not support waiting for synchronization."
	echo "The blockchain will be dumped without waiting until"
	echo "it is downloaded completely."
  echo ""
elif [[ $1 == '--wait' ]]; then
  EXECMODE="repl"
  echo "Auxx does not support waiting for synchronization."
  echo "As soon as the logs show that the node is synched,"
  echo "execute the following command:"
  echo ""
  echo "    dump $OUTPATH"
  echo ""
else
	echo "Usage: $0 [--wait|--nowait]"
  exit
fi;

stack exec -- cardano-auxx --mode=with-node               \
  $EXECMODE                                               \
  --no-ntp                                                \
  --topology $TOPOLOGY_FILE                               \
  $PEERS                                                  \
  --log-config ./scripts/log-templates/log-config-qa.yaml \
  --logs-prefix "logs/$CLUSTER"                           \
  --db-path "db-$CLUSTER"                                 \
  --keyfile secret-$CLUSTER.key                           \
  --configuration-file ./lib/configuration.yaml           \
  --configuration-key mainnet_full
