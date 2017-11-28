#!/usr/bin/env bash
set -e
set -o pipefail

SWAGGER_SPEC_PATH="wallet-new/spec/swagger.json"

stored_spec_hash=($(md5sum $SWAGGER_SPEC_PATH))
actual_spec_hash=($(stack exec wallet-new-swagger -- --dump | md5sum))

if [[ $stored_spec_hash == $actual_spec_hash ]]; then
    echo "Swagger doc is up-to-date"
else
    echo "Swagger spec $SWAGGER_SPEC_PATH is outdated, current md5 is \
         \\$stored_spec_hash, actual spec md5 is $actual_spec_hash"
    exit 1
fi

