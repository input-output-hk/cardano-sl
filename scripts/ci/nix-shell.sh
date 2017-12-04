#!/bin/sh

set -x

# Bootstrap on OS X/NixOS:
NIX_BUILD="$(type -P nix-build)"
NIX_BUILD="${NIX_BUILD:-/run/current-system/sw/bin/nix-build}"
NIX_SHELL="$(type -P nix-shell)"
NIX_SHELL="${NIX_SHELL:-/run/current-system/sw/bin/nix-shell}"
NIX_BUILD_SHELL="$(type -P bash)"
NIX_BUILD_SHELL="${NIX_BUILD_SHELL:-/run/current-system/sw/bin/bash}"

export NIX_REMOTE=daemon
export NIX_PATH="nixpkgs=$(${NIX_BUILD} fetch-nixpkgs.nix -o nixpkgs)"
export NIX_BUILD_SHELL

${NIX_SHELL} -p nix -p bash -p coreutils "$@"

