stack clean --full --nix
rm -rf .stack-wok
stack --nix clean cardano-sl
rm -rf run/* wallet-db/ *key daedalus/src/Generated/ db-abc/* node-*
