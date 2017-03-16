./util-scripts/clean.sh
CSL_SYSTEM_TAG=linux ./util-scripts/build-mac.sh --flag cardano-sl-core:-dev-mode
rm -rf daedalus/src/Generated/
stack --nix exec -- cardano-wallet-hs2purs
cd daedalus && npm install && npm run build:prod && npm link && npm start
