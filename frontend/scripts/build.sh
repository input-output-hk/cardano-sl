#!/usr/bin/env nix-shell
#! nix-shell -j 4 -i bash -p stack
#! nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/464c79ea9f929d1237dbc2df878eedad91767a72.tar.gz

set -xe
export SSL_CERT_FILE=$NIX_SSL_CERT_FILE
pushd ..
stack --nix build
stack --nix exec -- cardano-explorer-hs2purs --bridge-path ../src/Generated/
popd
nix-shell --run ./scripts/generate-backend-lenses.sh
nix-shell --run ./scripts/generate-frontend-lenses.sh
nix-shell --run "npm install && npm run build:prod"
echo "Done. Generated ./dist/"
