#!/usr/bin/env nix-shell
#! nix-shell -j 4 -i bash -p stack git
#! nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/763e21e982370f67c126f92a1113ea949db3b6e0.tar.gz

set -xe

if [ -n "$NIX_SSL_CERT_FILE" ]; then
  export SSL_CERT_FILE=$NIX_SSL_CERT_FILE
fi

pushd ..
if [ -n "$EXPLORER_NIX_FILE" ]; then
  $(nix-build -A cardano-sl-explorer-static $EXPLORER_NIX_FILE)/bin/cardano-explorer-hs2purs --bridge-path frontend/src/Generated/
else
  stack --nix build --fast --ghc-options="-j +RTS -A128m -n2m -RTS"
  stack --nix exec -- cardano-explorer-hs2purs --bridge-path frontend/src/Generated/
fi
popd
nix-shell --run "rm -rf .psci_modules/ .pulp-cache/ node_modules/ bower_components/ output/"
nix-shell --run "yarn install"
nix-shell --run ./scripts/generate-backend-lenses.sh
nix-shell --run ./scripts/generate-frontend-lenses.sh
nix-shell --run "yarn ${1:-build:prod}"
echo "Done."
