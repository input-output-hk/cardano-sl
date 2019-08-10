#!/usr/bin/env nix-shell
#!nix-shell -i bash -p nix-prefetch-git

set -euo pipefail

NIX_DIR=$(dirname "$0")

nix-prefetch-git https://github.com/input-output-hk/iohk-nix refs/heads/haskell-nix-cardano-sl \
                 > "$NIX_DIR/iohk-nix-src.json"
