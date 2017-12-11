#!/usr/bin/env bash
set -e
set -o pipefail

if [[ $TRAVIS_COMMIT == "" ]]; then
    echo "TRAVIS_COMMIT is unspecified"
    exit 1
fi

spec_path="https://raw.githubusercontent.com/input-output-hk/cardano-sl/$TRAVIS_COMMIT/wallet-new/spec/swagger.json"
validate="curl --fail http://online.swagger.io/validator/debug?url=$spec_path"

echo "Checking swagger spec at $spec_path"
errors=$($validate)

if [[ $errors == '{}' ]]; then
    echo "Swagger spec is valid"
else
    echo "Swagger spec is invalid!: $errors"
    exit 1
fi
