#!/usr/bin/env bash

set -e

if [[ ("$TRAVIS_OS_NAME" == "linux") && ("$TRAVIS_BRANCH" == "master") ]];
  then with_haddock=true
  else with_haddock=false
fi

targets="cardano-sl cardano-sl-auxx cardano-sl-tools cardano-sl-wallet cardano-sl-node cardano-sl-wallet-new"

# There are no macOS explorer devs atm and it's only deployed on linux
if [[ "$TRAVIS_OS_NAME" == "linux" ]]; then
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
  nix-build -A $trgt -o $trgt.root --argstr gitrev $TRAVIS_COMMIT
#    TODO: CSL-1133
#    if [[ "$trgt" == "cardano-sl" ]]; then
#      stack test --nix --fast --jobs=2 --coverage \
#      --ghc-options="-j +RTS -A128m -n2m -RTS";
#      stack --nix hpc report $to_build
#    fi

done

#if [[ "$TRAVIS_OS_NAME" == "linux" && "$TRAVIS_BRANCH" == "master" && "$TRAVIS_PULL_REQUEST" == "false" ]]; then
  #./update-wallet-web-api-docs.sh
  #./update-explorer-web-api-docs.sh
  #./update-cli-docs.sh
  #./update-haddock.sh
#fi

if [[ "$TRAVIS_OS_NAME" == "linux" ]]; then
  ./scripts/ci/check-relevancy-wallet-swagger.sh
fi

# Replace TRAVIS_BRANCH slash not to fail on subdirectory missing
export BUILD_UID="$TRAVIS_OS_NAME-${TRAVIS_BRANCH//\//-}"
export XZ_OPT=-1

# For now we dont have macOS developers on explorer
if [[ ("$TRAVIS_OS_NAME" == "linux") ]]; then
  # Generate explorer frontend
  EXPLORER_EXECUTABLE=$(pwd)/cardano-sl-explorer-static.root/bin/cardano-explorer-hs2purs ./explorer/frontend/scripts/build.sh

  echo "Packing up explorer-frontend ..."
  tar cJf s3/explorer-frontend-$BUILD_UID.tar.xz explorer/frontend/dist
  echo "Done"
fi
