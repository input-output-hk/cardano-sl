#!/usr/bin/env nix-shell
#!nix-shell -i bash -p bash

# regenerate the `pkgs/default.nix` file based on the current contents of cardano-sl.cabal and stack.yaml

function runInShell {
  nix-shell -j 4 -p nix cabal2nix glibcLocales --pure --run "LANG=en_US.utf-8 NIX_REMOTE=$NIX_REMOTE NIX_PATH=$NIX_PATH $*"
}

set -xe
set -v

# Get relative path to script directory
scriptDir="$(dirname -- "$(readlink -f -- "${BASH_SOURCE[0]}")")"

source "${scriptDir}/../scripts/set_nixpath.sh"

pushd "${scriptDir}"
  # https://github.com/NixOS/cabal2nix/issues/146
  runInShell cabal2nix --system x86_64-darwin --revision 25a53d417d7c7a8fc3116b63e3ba14ca7c8f188f \
     https://github.com/luite/hfsevents.git > hfsevents.nix

  # Generate cardano-sl package set
  runInShell $(nix-build -A stack2nix -Q ../)/bin/stack2nix --platform x86_64-linux --hackage-snapshot 2018-01-01T08:56:04Z -j8 --test ./.. > default.nix.new
  mv default.nix.new default.nix
popd
