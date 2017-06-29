#!/usr/bin/env bash

set -xe

fail_stack2nix_check() {
  echo "ERROR: you need to run ./pkgs/generate.sh and commit the changes"
  exit 1
}

# Get relative path to script directory
scriptDir=$(dirname -- "$(readlink -f -- "${BASH_SOURCE[0]}")")

source ${scriptDir}/../pkgs/generate.sh

git diff --text --exit-code || fail_stack2nix_check
