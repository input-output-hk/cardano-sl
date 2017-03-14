#!/usr/bin/env nix-shell
#! nix-shell -j 4 -i bash -p stack git
#! nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/464c79ea9f929d1237dbc2df878eedad91767a72.tar.gz

set -xe
export SSL_CERT_FILE=$NIX_SSL_CERT_FILE
pushd ..
stack --nix build --fast --ghc-options="-j +RTS -A128m -n2m -RTS"
stack --nix exec -- cardano-explorer-hs2purs --bridge-path frontend/src/Generated/
popd
nix-shell --run "npm install"
nix-shell --run ./scripts/generate-backend-lenses.sh
nix-shell --run ./scripts/generate-frontend-lenses.sh
nix-shell --run "npm run ${1:-build:prod}"
echo "Done."
