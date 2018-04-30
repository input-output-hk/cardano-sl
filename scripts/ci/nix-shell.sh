#!/bin/sh

set -x

case $(uname -s | tr A-Z a-z) in
        linux )  NIX_PROFILE_BINPATH=/run/current-system/sw/bin/;;
        darwin ) NIX_PROFILE_BINPATH=/nix/var/nix/profiles/default/bin;;
        * )      echo "Unsupported OS: $OS_NAME" >&2; exit 1;;
esac

# Bootstrap on OS X/NixOS:
NIX_BUILD="$(type -P nix-build)"
NIX_BUILD="${NIX_BUILD:-$NIX_PROFILE_BINPATH/nix-build}"
NIX_SHELL="$(type -P nix-shell)"
NIX_SHELL="${NIX_SHELL:-$NIX_PROFILE_BINPATH/nix-shell}"
NIX_BUILD_SHELL="$(type -P bash)"
NIX_BUILD_SHELL="${NIX_BUILD_SHELL:-$NIX_PROFILE_BINPATH/bash}"

export NIX_REMOTE=daemon
export NIX_PATH="nixpkgs=$(${NIX_BUILD} fetch-nixpkgs.nix -o nixpkgs)"
export NIX_BUILD_SHELL

${NIX_SHELL} -p nix bash coreutils zlib gmp ncurses "$@"
