rm -rf run/* wallet-db/ *key
set -ex
pushd cardano-sl
stack --nix build --flag cardano-sl:with-wallet --flag cardano-sl:with-web
stack --nix exec -- cardano-wallet-hs2purs
pushd daedalus
nix-shell --run "npm install && npm run build:prod"
popd
popd
cp cardano-sl/daedalus/dist/Daedalus.js app/api/CardanoBridge.js
