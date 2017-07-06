#!/bin/sh

set -e

rm -rf frontend/src/Generated
stack exec -- cardano-explorer-hs2purs --bridge-path ./frontend/src/Generated

cd frontend
rm -rf output node_modules bower_components
npm install # or use yarn install

scripts/generate-backend-lenses.sh
scripts/generate-frontend-lenses.sh

npm start # or use yarn start

# open http://localhost:3100/
