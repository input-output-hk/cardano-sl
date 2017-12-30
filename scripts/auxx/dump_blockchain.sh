#!/usr/bin/env bash

TOPOLOGY_FILE=/tmp/topology-mainnet.yaml
CLUSTER=mainnet
RELAY=relays.cardano-mainnet.iohk.io
OUTPATH=/tmp/cardano-$CLUSTER-blockchain_$(date +%Y-%m-%d_%H-%M-%S)

PEERS=$(\
    nslookup $RELAY | \
    awk '/^Address: / { print "--peer", $2 }' | \
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
elif [[ $1 == '--wait' ]]; then
	EXECMODE="repl"
	echo "Auxx does not support waiting for synchronization."
	echo "As soon as the logs show that the node is synched,"
	echo "execute the following command:"
	echo ""
	echo "    dump $OUTPATH"
else
	echo "Usage: $0 [--wait|--nowait]"
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
