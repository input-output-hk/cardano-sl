stack clean --full
rm -rf .stack-wok
stack clean cardano-sl
rm -rf run/* wallet-db/ *key daedalus/src/Generated/
stack build
stack exec -- cardano-wallet-hs2purs
cd daedalus && npm install && npm run build:prod && npm link
