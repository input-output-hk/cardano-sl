#!/usr/bin/env bash

# Regenerate the `pkgs/default.nix` file based on the current
# contents of cardano-sl.cabal and stack.yaml, with a hackage snapshot.

set -euo pipefail
git submodule update --init
cd "$(dirname -- "$(readlink -f -- "${BASH_SOURCE[0]}")")"
exec "$(nix-build --no-out-link regen.nix)" "$@"
