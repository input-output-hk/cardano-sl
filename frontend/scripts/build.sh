#!/usr/bin/env nix-shell
#! nix-shell -j 4 -i bash -p git stack
#! nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/464c79ea9f929d1237dbc2df878eedad91767a72.tar.gz

set -xe
git submodule update --init --remote --recursive
pushd cardano-sl-explorer
stack --nix build
stack --nix exec -- cardano-explorer-hs2purs --bridge-path ../src/Generated/
popd
nix-shell --run ./scripts/generate-backend-lenses.sh
nix-shell --run ./scripts/generate-frontend-lenses.sh
nix-shell --run "npm install && npm run build:prod"
echo "Done. Generated ./dist/"
