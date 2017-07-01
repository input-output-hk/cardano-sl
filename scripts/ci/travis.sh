#!/usr/bin/env bash

set -e

export EXTRA_STACK="--no-haddock-deps"

if [[ ("$TRAVIS_OS_NAME" == "linux") && ("$TRAVIS_BRANCH" == "master") ]];
  then with_haddock=true
  else with_haddock=false
fi

if [[ "$TRAVIS_OS_NAME" == "linux" ]]; then
    if [[ "$with_haddock" == "true" ]]; then
      # export EXTRA_STACK="--haddock";
      :
    fi

    export EXTRA_STACK="--test $EXTRA_STACK";
fi

if [[ "$TRAVIS_OS_NAME" == "osx" ]]; then
  export EXTRA_STACK="--flag cardano-sl:for-installer $EXTRA_STACK"
fi

stack --nix --no-terminal install happy \
  $EXTRA_STACK --fast --ghc-options="-j +RTS -A128m -n2m -RTS" --jobs=4

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

for trgt in $targets; do

    stack --nix --no-terminal --local-bin-path daedalus/ install "$trgt" \
      $EXTRA_STACK --fast --jobs=2 \
      --ghc-options="-j -DCONFIG=$DCONFIG +RTS -A128m -n2m -RTS" \
      --flag cardano-sl-core:-asserts \
      --flag cardano-sl-core:-dev-mode

done

#if [[ "$TRAVIS_OS_NAME" == "linux" && "$TRAVIS_BRANCH" == "master" && "$TRAVIS_PULL_REQUEST" == "false" ]]; then
  #./update-wallet-web-api-docs.sh
  #./update-cli-docs.sh
  #./update-haddock.sh
#fi

stack exec --nix -- cardano-wallet-hs2purs

pushd daedalus
  nix-shell --run "npm install && npm run build:prod"
  echo $TRAVIS_BUILD_NUMBER > build-id
  cp ../log-config-prod.yaml .
popd

# Replace TRAVIS_BRANCH slash not to fail on subdirectory missing
echo "Packing up daedalus-bridge ..."
XZ_OPT=-1 tar cJf s3/daedalus-bridge-$TRAVIS_OS_NAME-${TRAVIS_BRANCH//\//-}.tar.xz daedalus/
echo "Done"
