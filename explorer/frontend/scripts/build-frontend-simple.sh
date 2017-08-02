#!/bin/sh

set -e

if ! which purescript-derive-lenses >/dev/null; then
    echo "Installing purescript-derive-lenses..."
    rm -rf purescript-derive-lenses
    git clone https://github.com/paf31/purescript-derive-lenses
    cd purescript-derive-lenses
    git checkout 02457e6 # TODO: I am not sure which version should we use? @jens?
    stack install --install-ghc
    cd ..
    rm -rf purescript-derive-lenses
fi

rm -rf frontend/src/Generated
stack exec -- cardano-explorer-hs2purs --bridge-path ./frontend/src/Generated

cd frontend
rm -rf output node_modules bower_components
yarn install # or use npm install

scripts/generate-backend-lenses.sh
scripts/generate-frontend-lenses.sh

yarn start # or use npm start

# open http://localhost:3100/
