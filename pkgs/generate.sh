#!/usr/bin/env nix-shell
#!nix-shell -i bash -p bash

# regenerate the `pkgs/default.nix` file based on the current contents of cardano-sl.cabal and stack.yaml

function runInShell {
  nix-shell -j 4 -p cabal2nix nix-prefetch-scripts coreutils cabal-install stack --run "$*"
}

set -xe
set -v

# Get relative path to script directory
scriptDir="$(dirname -- "$(readlink -f -- "${BASH_SOURCE[0]}")")"

source "${scriptDir}/../scripts/set_nixpath.sh"


pushd "${scriptDir}"
  # Build stack2nix Nix package
  nix-build -A stack2nix -o stack2nix -Q ../

  # https://github.com/NixOS/cabal2nix/issues/146
  runInShell cabal2nix --system x86_64-darwin --revision 25a53d417d7c7a8fc3116b63e3ba14ca7c8f188f \
     https://github.com/luite/hfsevents.git > hfsevents.nix

  # Generate cardano-sl package set
  runInShell ./stack2nix/bin/stack2nix --test ./.. > default.nix.new
  mv default.nix.new default.nix
popd
