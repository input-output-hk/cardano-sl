#!/usr/bin/env bash

set -xe

# shellcheck disable=SC1091
source scripts/set_nixpath.sh

OS_NAME=$(uname -s | tr '[:upper:]' '[:lower:]')

if [[ ("$OS_NAME" == "linux") && ("$BUILDKITE_BRANCH" == "master") ]];
  then with_haddock=true
  else with_haddock=false
fi
export with_haddock

targets="cardano-sl cardano-sl-auxx cardano-sl-tools cardano-sl-wallet cardano-sl-wallet-new daedalus-bridge"

# There are no macOS explorer devs atm and it's only deployed on linux
if [[ "$OS_NAME" == "linux" ]]; then
   targets="$targets cardano-sl-explorer-static cardano-sl-explorer-frontend"
fi

# TODO: CSL-1133: Add test coverage to CI. To be reenabled when build times
# become smaller and allow coverage report to be built.

for trgt in $targets; do
  echo "Building $trgt verbosely.."
  nix-build -A "$trgt" -o "$trgt.root" \
      --argstr gitrev "$BUILDKITE_COMMIT" \
      --argstr buildId "$BUILDKITE_BUILD_NUMBER" \
      --arg enableBenchmarks true

#    TODO: CSL-1133
#    if [[ "$trgt" == "cardano-sl" ]]; then
#      stack test --nix --fast --jobs=2 --coverage \
#      --ghc-options="-j +RTS -A128m -n2m -RTS";
#      stack --nix hpc report $to_build
#    fi

done
