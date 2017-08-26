#!/usr/bin/env bash

# check and warn if `pkgs/default.nix` is out of date

set -xe

fail_stack2nix_check() {
  GIST=$(nix-build -Q '<nixpkgs>' -A gist)
  git diff --text > /tmp/stack2nix.patch
  gisturl=$(${GIST}/bin/gist -p /tmp/stack2nix.patch)
  echo "ERROR: you need to (run ./pkgs/generate.sh or apply the patch in $gisturl) and commit the changes" >&2
  exit 1
}

# Get relative path to script directory
scriptDir=$(dirname -- "$(readlink -f -- "${BASH_SOURCE[0]}")")

source ${scriptDir}/../pkgs/generate.sh


git diff --text --exit-code || fail_stack2nix_check
