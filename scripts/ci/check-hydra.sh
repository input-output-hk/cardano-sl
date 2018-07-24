#!/bin/sh

set -e
set -o pipefail

echo '~~~ Fetching hydra'
nix-build https://github.com/nixos/nixpkgs/archive/4fb198892d298452023ab176e7067da58d30772e.tar.gz -A hydra -o oldhydra
nix-build https://github.com/nixos/nixpkgs/archive/43c77db3aa58e06cc3ced846431dd4228f93cd5d.tar.gz -A hydra -o newhydra
nix-build -E '(import (import ./lib.nix).fetchNixPkgs {}).jq' -o jq

export PATH=./jq/bin/:$PATH

function checkJson() {
  jq 'to_entries | map(select(.value.error? != null)) | map(error(.value.error))'
}

echo '~~~ Evaluating release.nix with old hydra'
./oldhydra/bin/hydra-eval-jobs -I . release.nix | checkJson

echo '~~~ Evaluating release.nix with new hydra'
./newhydra/bin/hydra-eval-jobs -I . release.nix --show-trace --option allowed-uris https://github.com/NixOS/nixpkgs/archive | checkJson
