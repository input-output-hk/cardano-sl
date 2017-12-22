#!/usr/bin/env bash

set -xe

source scripts/set_nixpath.sh

OS_NAME=$(uname -s | tr A-Z a-z)

if [[ ("$OS_NAME" == "linux") && ("$BUILDKITE_BRANCH" == "master") ]];
  then with_haddock=true
  else with_haddock=false
fi

targets="cardano-sl cardano-sl-auxx cardano-sl-tools cardano-sl-wallet"

# There are no macOS explorer devs atm and it's only deployed on linux
if [[ "$OS_NAME" == "linux" ]]; then
   targets="$targets cardano-sl-explorer-static"
fi

# TODO: CSL-1133: Add test coverage to CI. To be reenabled when build times
# become smaller and allow coverage report to be built.
#projects="core db lrc infra update ssc godtossing txp"
#to_build=''

#for prj in $projects; do
#  to_build="$to_build cardano-sl-$prj"
#done

for trgt in $targets; do
  echo "Prebuilding dependencies for $trgt, quietly.."
  nix-shell -A $trgt --run true --no-build-output --cores 0 --max-jobs 4 default.nix ||
          echo "Prebuild failed!"
          
  echo "Building $trgt verbosely.."
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
  echo $BUILDKITE_BUILD_NUMBER > build-id
  echo $BUILDKITE_COMMIT > commit-id
  cp ../log-config-prod.yaml .
  cp ../node/configuration.yaml .
  cp ../node/*genesis*.json .
  cp ../cardano-sl-tools.root/bin/cardano-launcher .
  cp ../cardano-sl-wallet.root/bin/cardano-node .
  # check that binaries exit with 0
  ./cardano-node --help > /dev/null
  ./cardano-launcher --help > /dev/null
popd

# Replace BUILDKITE_BRANCH slash not to fail on subdirectory missing
export BUILD_UID="$OS_NAME-${BUILDKITE_BRANCH//\//-}"
export XZ_OPT=-1

echo "Packing up daedalus-bridge ..."
tar cJf cardano-binaries-$BUILD_UID.tar.xz daedalus/
echo "Done"
buildkite-agent artifact upload "cardano-binaries-$BUILD_UID.tar.xz"   --job $BUILDKITE_JOB_ID

# For now we dont have macOS developers on explorer
if [[ ("$OS_NAME" == "linux") ]]; then
  # Generate explorer frontend
  export EXPLORER_EXECUTABLE=$(pwd)/cardano-sl-explorer-static.root/bin/cardano-explorer-hs2purs
  ./explorer/frontend/scripts/build.sh

  echo "Packing up explorer-frontend ..."
  tar cJf explorer-frontend-$BUILD_UID.tar.xz explorer/frontend/dist
  echo "Done"
  buildkite-agent artifact upload "explorer-frontend-$BUILD_UID.tar.xz" s3://ci-output-sink --job $BUILDKITE_JOB_ID
fi

buildkite-agent artifact upload "daedalus-bridge-$BUILD_UID.tar.xz"   s3://ci-output-sink --job $BUILDKITE_JOB_ID
