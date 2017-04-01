./util-scripts/clean.sh
stack build
stack exec -- cardano-wallet-hs2purs
cd daedalus && npm install && npm run build:prod && npm link
