#!/usr/bin/env bash

set -e

source scripts/set_nixpath.sh

OS_NAME=$(uname -s | tr A-Z a-z)

if [[ ("$OS_NAME" == "linux") && ("$BUILDKITE_BRANCH" == "master") ]];
  then with_haddock=true
  else with_haddock=false
fi

targets="cardano-sl cardano-sl-auxx cardano-sl-tools cardano-sl-wallet cardano-sl-node"

# There are no macOS explorer devs atm and it's only deployed on linux
if [[ "$OS_NAME" == "linux" ]]; then
   targets="$targets cardano-sl-explorer-static"
fi

# TODO: CSL-1133: Add test coverage to CI. To be reenabled when build times
# become smaller and allow coverage report to be built.
#projects="core db lrc infra update ssc txp"
#to_build=''

#for prj in $projects; do
#  to_build="$to_build cardano-sl-$prj"
#done

for trgt in $targets; do
  echo building $trgt with nix
  nix-build -A $trgt -o $trgt.root --argstr gitrev $BUILDKITE_COMMIT
#    TODO: CSL-1133
#    if [[ "$trgt" == "cardano-sl" ]]; then
#      stack test --nix --fast --jobs=2 --coverage \
#      --ghc-options="-j +RTS -A128m -n2m -RTS";
#      stack --nix hpc report $to_build
#    fi

done

#if [[ "$OS_NAME" == "linux" && "$BUILDKITE_BRANCH" == "master" && "$BUILDKITE_PULL_REQUEST" == "false" ]]; then
  #./update-wallet-web-api-docs.sh
  #./update-explorer-web-api-docs.sh
  #./update-cli-docs.sh
  #./update-haddock.sh
#fi

./cardano-sl-wallet.root/bin/cardano-wallet-hs2purs

# Generate daedalus-bridge
pushd daedalus
  nix-shell --run "npm install && npm run build:prod"
  echo $BUILDKITE_BUILD_NUMBER > build-id
  echo $BUILDKITE_COMMIT > commit-id
  echo https://buildkite.com/input-output-hk/cardano-sl/builds/$BUILDKITE_BUILD_NUMBER > ci-url
  cp ../log-config-prod.yaml .
  cp ../lib/configuration.yaml .
  cp ../lib/*genesis*.json .
  cp ../cardano-sl-tools.root/bin/cardano-launcher .
  cp ../cardano-sl-wallet.root/bin/cardano-node .
  # check that binaries exit with 0
  ./cardano-node --help > /dev/null
  ./cardano-launcher --help > /dev/null
popd

# Replace BUILDKITE_BRANCH slash not to fail on subdirectory missing
export BUILD_UID="$OS_NAME-${BUILDKITE_BRANCH//\//-}"
export XZ_OPT=-1

mkdir -p s3

echo "Packing up daedalus-bridge ..."
tar cJf s3/daedalus-bridge-$BUILD_UID.tar.xz daedalus/
echo "Done"

# For now we dont have macOS developers on explorer
if [[ ("$OS_NAME" == "linux") ]]; then
  # Generate explorer frontend
  EXPLORER_EXECUTABLE=$(pwd)/cardano-sl-explorer-static.root/bin/cardano-explorer-hs2purs ./explorer/frontend/scripts/build.sh

  echo "Packing up explorer-frontend ..."
  tar cJf s3/explorer-frontend-$BUILD_UID.tar.xz explorer/frontend/dist
  echo "Done"
fi
