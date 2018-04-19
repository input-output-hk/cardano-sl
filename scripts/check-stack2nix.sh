#!/usr/bin/env bash

# check and warn if `pkgs/default.nix` is out of date

set -xe

fail_stack2nix_check() {
  # The '-w' option for 'git diff' is used because in some builds
  # (i.e. https://buildkite.com/input-output-hk/cardano-sl/builds/976#0fb162df-8f9b-42d7-9ca7-608a9ea06d4d)
  # the patch to 'default.nix' only suggests that whitespaces be added
  # (see https://gist.github.com/anonymous/f52dbb040db16034d303e27056a0a48e), without
  # which the build fails in the 'stack2nix' step.
  git diff -w --text > /tmp/stack2nix.patch
  buildkite-agent artifact upload /tmp/stack2nix.patch --job $BUILDKITE_JOB_ID
  echo "ERROR: you need to (run ./pkgs/generate.sh or apply the patch in the buildkite artifact) and commit the changes" >&2
  exit 1
}

# Get relative path to script directory
scriptDir=$(dirname -- "$(readlink -f -- "${BASH_SOURCE[0]}")")

source ${scriptDir}/../pkgs/generate.sh


git diff -w --text --exit-code || fail_stack2nix_check
