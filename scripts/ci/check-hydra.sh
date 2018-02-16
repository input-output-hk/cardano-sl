#!/bin/sh

nix-build https://github.com/nixos/nixpkgs/archive/4fb198892d298452023ab176e7067da58d30772e.tar.gz -A hydra
echo '~~~ Evaluating release.nix'
./result/bin/hydra-eval-jobs -I . release.nix
