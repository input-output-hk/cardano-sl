#!/bin/sh

set -x
set -e

nix-build fetch-nixpkgs.nix
nix-build -E 'import (import ./fetch-nixpkgs.nix) { config = {}; }' -A hydra
echo '~~~ Evaluating release.nix'
./result/bin/hydra-eval-jobs -I . --option allowed-uris 'https://github.com/NixOS/nixpkgs/archive/ https://github.com/input-output-hk/stack2nix/archive' release.nix --show-trace
