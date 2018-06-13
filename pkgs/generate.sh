#!/usr/bin/env nix-shell
#!nix-shell -i bash -p bash

# regenerate the `pkgs/default.nix` file based on the current contents of cardano-sl.cabal and stack.yaml

function runInShell {
  local inputs="$1"
  shift
  nix-shell -j 4 -E "with import (import $scriptDir/../fetch-nixpkgs.nix) {}; runCommand \"shell\" { buildInputs = [ $inputs ]; } \"\"" --run "LANG=en_US.utf-8 NIX_REMOTE=$NIX_REMOTE $*"
}

set -xe
set -v

# Ensure that nix 1.11 (used by cabal2nix) works in multi-user mode.
if [ ! -w /nix/store ]; then
    export NIX_REMOTE=${NIX_REMOTE:-daemon}
fi

# Get relative path to script directory
scriptDir="$(dirname -- "$(readlink -f -- "${BASH_SOURCE[0]}")")"

pushd "${scriptDir}"

  # https://github.com/NixOS/cabal2nix/issues/146
  runInShell "cabal2nix glibcLocales" cabal2nix --system x86_64-darwin --revision 25a53d417d7c7a8fc3116b63e3ba14ca7c8f188f \
     https://github.com/luite/hfsevents.git > hfsevents.nix

  # Generate cardano-sl package set
  runInShell "cabal2nix glibcLocales" "$(nix-build -A stack2nix --no-out-link -Q ../)/bin/stack2nix" --platform x86_64-linux --hackage-snapshot 2018-06-10T09:58:14Z -j8 --test --bench --no-indent ./.. > default.nix.new
  mv default.nix.new default.nix
popd
