#!/bin/sh

nix-build '<nixpkgs>' -A hydra
echo '~~~ Evaluation release.nix'
./result/bin/hydra-eval-jobs -I . release.nix
