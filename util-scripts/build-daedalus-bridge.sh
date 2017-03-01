stack clean --full --nix
rm -rf .stack-wok
stack --nix clean cardano-sl
rm -rf run/* wallet-db/ *key daedalus/src/Generated/
stack --nix build
stack --nix exec -- cardano-wallet-hs2purs
cd daedalus && npm install && npm run build:prod && npm link
