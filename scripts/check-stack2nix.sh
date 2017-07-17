#!/usr/bin/env bash

# check and warn if `pkgs/default.nix` is out of date

set -xe

fail_stack2nix_check() {
  echo "ERROR: you need to run ./pkgs/generate.sh and commit the changes" >&2
  exit 1
}

# Get relative path to script directory
scriptDir=$(dirname -- "$(readlink -f -- "${BASH_SOURCE[0]}")")

source ${scriptDir}/../pkgs/generate.sh

git diff --text --exit-code || fail_stack2nix_check
