# this is a bit faster script then build-daedalus-bridge.sh
rm -rf run/* wallet-db/ *key daedalus/src/Generated/ db-abc/* node-* daedalus/.psci_modules/ daedalus/.pulp-cache/ daedalus/node_modules/ daedalus/bower_components/ daedalus/output/
./util-scripts/build.sh
rm -rf kademlia-abc.dump
stack exec -- cardano-wallet-hs2purs
cd daedalus && npm install && npm run build:prod
