./util-scripts/clean.sh
./util-scripts/build.sh
stack exec -- cardano-wallet-hs2purs
cd daedalus && npm install && npm run build:prod && npm link
