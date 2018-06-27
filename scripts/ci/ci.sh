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

# Build everything on linux but only a limited set of packages and
# tests on macos.
if [[ "$OS_NAME" == "linux" ]]; then
    targets="all-cardano-sl daedalus-bridge"
else
    targets="daedalus-bridge"
fi

# TODO: CSL-1133: Add test coverage to CI. To be reenabled when build times
# become smaller and allow coverage report to be built.

for trgt in $targets; do
  echo "Building $trgt..."
  nix-build -A "$trgt" -o "$trgt.root" \
      --argstr gitrev "$BUILDKITE_COMMIT" \
      --argstr buildId "$BUILDKITE_BUILD_NUMBER" \
      --arg enableBenchmarks true
done
