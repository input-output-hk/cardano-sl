#!/usr/bin/env bash

# regenerate the `pkgs/default.nix` file based on the current contents of cardano-sl.cabal and stack.yaml

function runInShell {
  nix-shell -j 4 -p cabal2nix nix-prefetch-scripts coreutils cabal-install stack --run "$*"
}

set -xe
set -v

# Get relative path to script directory
scriptDir="$(dirname -- "$(readlink -f -- "${BASH_SOURCE[0]}")")"

source "${scriptDir}/../scripts/set_nixpath.sh"

# Generate stack2nix Nix package
runInShell cabal2nix \
  --no-check \
  --revision $(jq .rev <  "${scriptDir}/../stack2nix-src.json" -r) \
  https://github.com/input-output-hk/stack2nix.git > "$scriptDir/stack2nix.nix"

# Build stack2nix Nix package
nix-build "${scriptDir}/.." -A stack2nix -o "$scriptDir/stack2nix" -Q

pushd "${scriptDir}"
# Generate cardano-sl package set
runInShell ./stack2nix/bin/stack2nix ./.. > default.nix.new
mv default.nix.new default.nix

popd
