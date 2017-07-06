#!/bin/sh

set -e

rm -rf frontend/src/Generated
stack exec -- cardano-explorer-hs2purs --bridge-path ./frontend/src/Generated

cd frontend
rm -rf output node_modules bower_components
yarn install # or use npm install

scripts/generate-backend-lenses.sh
scripts/generate-frontend-lenses.sh

yarn start # or use npm start

# open http://localhost:3100/
