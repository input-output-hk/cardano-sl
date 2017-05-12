#!/usr/bin/env bash

set -e

EXTRA_STACK="--no-haddock-deps"

case "$TRAVIS_OS_NAME" in
  linux )
    if [[ "$TRAVIS_BRANCH" == "master" ]]; then
      EXTRA_STACK="--haddock";
    fi

    EXTRA_STACK="--test $EXTRA_STACK";;
  osx )
    EXTRA_STACK="--flag cardano-sl:for-installer $EXTRA_STACK";;
  * )
    echo "FATAL: unexpected value of \$TRAVIS_OS_NAME: '$TRAVIS_OS_NAME'"
    exit 1;;
esac

export EXTRA_STACK

stack --nix --no-terminal --local-bin-path daedalus/ install cardano-sl \
  $EXTRA_STACK --fast --ghc-options="-j +RTS -A128m -n2m -RTS" --jobs=4 \
  --flag cardano-sl:-asserts \
  --flag cardano-sl-core:-dev-mode | ts

if [[ "$TRAVIS_OS_NAME" == "linux" && "$TRAVIS_BRANCH" == "master" ]]; then
  ./update_wallet_web_api_docs.sh
  ./update_haddock.sh
fi

stack exec --nix -- cardano-wallet-hs2purs
stack exec --nix -- cardano-wallet-web-docs

pushd daedalus
  nix-shell --run "npm install && npm run build:prod"
  echo $TRAVIS_BUILD_NUMBER > build-id
  cp ../log-config-prod.yaml .
popd

# Replace TRAVIS_BRANCH slash not to fail on subdirectory missing
echo "Packing up daedalus-bridge ..."
XZ_OPT=-1 tar cJf s3/daedalus-bridge-$TRAVIS_OS_NAME-${TRAVIS_BRANCH/\//-}.tar.xz daedalus/
echo "Done"
