#!/usr/bin/env bash

set -e

if [[ ("$TRAVIS_OS_NAME" == "linux") && ("$TRAVIS_BRANCH" == "master") ]];
  then with_haddock=true
  else with_haddock=false
fi

if [[ "$TRAVIS_OS_NAME" == "linux" ]]; then
    if [[ "$with_haddock" == "true" ]]; then

      # We need to pass CONFIG=wallet to compile cardano-sl-core, but Stack doesn't
      # support passing arguments to Haddock yet (this will be fixed in the next
      # release after 1.4.0). For now, a workaround is to manually replace CONFIG
      # with "wallet" in *.hs files.
      #
      # When new Stack is released, delete this and add an argument to Stack:
      #    --haddock-arguments="--optghc=-DCONFIG=wallet"
      find core/ -name '*.hs' -exec sed -i 's/defined(CONFIG)/1/g' {} +
      find core/ -name '*.hs' -exec sed -i 's/QUOTED(CONFIG)/"'$DCONFIG'"/g' {} +
    fi

fi

# We need to pass CONFIG=wallet to compile cardano-sl-core, but Stack doesn't
# support passing arguments to Haddock yet (this will be fixed in the next
# release after 1.4.0). For now, a workaround is to manually replace CONFIG
# with "wallet" in *.hs files.
#
# When new Stack is released, delete this and add an argument to Stack:
#    --haddock-arguments="--optghc=-DCONFIG=wallet"
if [[ "$with_haddock" == "true" ]]; then
  find core/ -name '*.hs' -exec sed -i 's/defined(CONFIG)/1/g' {} +
  find core/ -name '*.hs' -exec sed -i 's/QUOTED(CONFIG)/"'$DCONFIG'"/g' {} +
fi

targets="cardano-sl cardano-sl-lwallet cardano-sl-tools"

# TODO: CSL-1133: Add test coverage to CI. To be reenabled when build times
# become smaller and allow coverage report to be built.
#projects="core db lrc infra update ssc godtossing txp"
#to_build=''

#for prj in $projects; do
#  to_build="$to_build cardano-sl-$prj"
#done

for trgt in $targets; do
  echo building $trgt with nix
  nix-build -A $trgt -o $trgt.root --argstr dconfig $DCONFIG
#    TODO: CSL-1133
#    if [[ "$trgt" == "cardano-sl" ]]; then
#      stack test --nix --fast --jobs=2 --coverage \
#      --ghc-options="-j -DCONFIG=$DCONFIG +RTS -A128m -n2m -RTS";
#      stack --nix hpc report $to_build
#    fi

done

#if [[ "$TRAVIS_OS_NAME" == "linux" && "$TRAVIS_BRANCH" == "master" && "$TRAVIS_PULL_REQUEST" == "false" ]]; then
  #./update-wallet-web-api-docs.sh
  #./update-cli-docs.sh
  #./update-haddock.sh
#fi

./cardano-sl-tools.root/bin/cardano-wallet-hs2purs

pushd daedalus
  nix-shell --run "npm install && npm run build:prod"
  echo $TRAVIS_BUILD_NUMBER > build-id
  cp ../log-config-prod.yaml .
popd

# Replace TRAVIS_BRANCH slash not to fail on subdirectory missing
echo "Packing up daedalus-bridge ..."
XZ_OPT=-1 tar cJf s3/daedalus-bridge-$TRAVIS_OS_NAME-${TRAVIS_BRANCH//\//-}.tar.xz daedalus/
echo "Done"
