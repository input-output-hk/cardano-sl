#!/usr/bin/env bash

set -xe

source scripts/set_nixpath.sh

OS_NAME=$(uname -s | tr A-Z a-z)

if [[ ("$OS_NAME" == "linux") && ("$BUILDKITE_BRANCH" == "master") ]];
  then with_haddock=true
  else with_haddock=false
fi

targets="cardano-sl cardano-sl-auxx cardano-sl-tools cardano-sl-wallet cardano-sl-wallet-new daedalus-bridge"

# There are no macOS explorer devs atm and it's only deployed on linux
if [[ "$OS_NAME" == "linux" ]]; then
   targets="$targets cardano-sl-explorer-static cardano-sl-explorer-frontend"
fi

# TODO: CSL-1133: Add test coverage to CI. To be reenabled when build times
# become smaller and allow coverage report to be built.
#projects="core db lrc infra update ssc godtossing txp"
#to_build=''

#for prj in $projects; do
#  to_build="$to_build cardano-sl-$prj"
#done

for trgt in $targets; do
  # echo "Prebuilding dependencies for $trgt, quietly.."
  # nix-shell -A $trgt --run true --no-build-output --cores 0 --max-jobs 4 default.nix ||
  #         echo "Prebuild failed!"

  echo "Building $trgt verbosely.."
  nix-build -A $trgt -o $trgt.root --argstr gitrev $BUILDKITE_COMMIT --argstr buildId $BUILDKITE_BUILD_NUMBER
#    TODO: CSL-1133
#    if [[ "$trgt" == "cardano-sl" ]]; then
#      stack test --nix --fast --jobs=2 --coverage \
#      --ghc-options="-j +RTS -A128m -n2m -RTS";
#      stack --nix hpc report $to_build
#    fi

done

#if [[ "$OS_NAME" == "linux" && "$BUILDKITE_BRANCH" == "master" && "$BUILDKITE_PULL_REQUEST" == "false" ]]; then
  # XXX: DEVOPS-728 this won't work, unless `GITHUB_CARDANO_DOCS_ACCESS_2` and `GITHUB_CARDANO_DOCS_ACCESS` vars are supplied
  #
  #./update-wallet-web-api-docs.sh
  #./update-explorer-web-api-docs.sh
  #./update-cli-docs.sh
  #./update-haddock.sh
#fi
