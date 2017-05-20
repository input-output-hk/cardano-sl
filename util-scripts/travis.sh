#!/usr/bin/env bash

set -em -o xtrace

export EXTRA_STACK="--no-haddock-deps"

if [[ "$TRAVIS_OS_NAME" == "linux" ]]; then
    if [[ "$TRAVIS_BRANCH" == "master" ]]; then
      export EXTRA_STACK="--haddock";
    fi

    export EXTRA_STACK="--test $EXTRA_STACK";
fi

if [[ "$TRAVIS_OS_NAME" == "osx" ]]; then
  export EXTRA_STACK="--flag cardano-sl:for-installer $EXTRA_STACK"
fi

mkdir -p .stack-work/logs
touch $(stack --nix list-dependencies --separator '-' | sed 's/$/.log/' | sed 's/^/.stack-work\/logs\//')
tail -f .stack-work/logs/* &

stack --nix --no-terminal install happy \
  $EXTRA_STACK --fast --ghc-options="+RTS -M7G -RTS" --jobs=4

free -h

stack --nix --no-terminal --local-bin-path daedalus/ install cardano-sl \
  $EXTRA_STACK --fast --ghc-options="+RTS -M7G -RTS" --jobs=2 \
  --flag cardano-sl:-asserts \
  --flag cardano-sl-core:-dev-mode | ts

if [[ "$TRAVIS_OS_NAME" == "linux" && "$TRAVIS_BRANCH" == "master" && "$TRAVIS_PULL_REQUEST" == "false" ]]; then
  ./update_wallet_web_api_docs.sh
  ./update_haddock.sh
fi

stack exec --nix -- cardano-wallet-hs2purs

pushd daedalus
  nix-shell --run "npm install && npm run build:prod"
  echo $TRAVIS_BUILD_NUMBER > build-id
  cp ../log-config-prod.yaml .
popd

# Replace TRAVIS_BRANCH slash not to fail on subdirectory missing
echo "Packing up daedalus-bridge ..."
XZ_OPT=-1 tar cJf s3/daedalus-bridge-$TRAVIS_OS_NAME-${TRAVIS_BRANCH/\//-}.tar.xz daedalus/
echo "Done"

kill %1
