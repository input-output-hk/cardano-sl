stack clean --full --nix
rm -rf .stack-wok
stack --nix clean cardano-sl
rm -rf run/* wallet-db/ *key daedalus/src/Generated/ db-abc/* node-* daedalus/.psci_modules/ daedalus/.pulp-cache/ daedalus/node_modules/ daedalus/bower_components/ daedalus/output/
