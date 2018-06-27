#!/usr/bin/env bash

# check and warn if commits don't conform to stylish-haskell

set -xe

fail_stylish_check() {
  git diff --text > /tmp/stylish.patch
  buildkite-agent artifact upload /tmp/stylish.patch --job "$BUILDKITE_JOB_ID"
  echo "ERROR: you need to (run ./scripts/haskell/stylish-last-10.sh or apply the patch in the buildkite artifact) and commit the changes" >&2
  exit 1
}

# Get relative path to script directory
haskellScriptDir=$(dirname -- "$(readlink -f -- "${BASH_SOURCE[0]}")")/haskell/

source "${haskellScriptDir}stylish-last-10.sh"

git diff --text --exit-code || fail_stylish_check
