#!/usr/bin/env nix-shell
#! nix-shell -i bash -p bash

# Get relative path to script directory
scriptDir="$(dirname -- "$(readlink -f -- "${BASH_SOURCE[0]}")")"
source "${scriptDir}/../../../scripts/set_nixpath.sh"


if [ -n "$NIX_SSL_CERT_FILE" ]; then
  export SSL_CERT_FILE=$NIX_SSL_CERT_FILE
fi

ARGS="--bridge-path frontend/src/Generated/"

set -xe
set -v

pushd $scriptDir/../..
  echo EXPLORER_NIX_FILE=$EXPLORER_NIX_FILE
  echo EXPLORER_EXECUTABLE=$EXPLORER_EXECUTABLE
  if [ -n "$EXPLORER_NIX_FILE" ]; then
    $(nix-build -A cardano-sl-explorer-static $EXPLORER_NIX_FILE)/bin/cardano-explorer-hs2purs $ARGS
  elif [ -n "$EXPLORER_EXECUTABLE" ]; then
    $EXPLORER_EXECUTABLE $ARGS
  else
    stack --nix install happy --fast --ghc-options="-j +RTS -A128m -n2m -RTS"
    stack --nix build --fast --ghc-options="-j +RTS -A128m -n2m -RTS"
    stack --nix exec -- cardano-explorer-hs2purs $ARGS
  fi
popd

pushd $scriptDir/..
  nix-shell --run "rm -rf .psci_modules/ .pulp-cache/ node_modules/ bower_components/ output/"
  nix-shell --run "yarn install"
  nix-shell --run ./scripts/generate-explorer-lenses.sh
  nix-shell --run "yarn run ${1:-build:prod}"
popd

echo "Done."
