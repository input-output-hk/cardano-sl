#!/usr/bin/env bash

if ! [ -x "$(command -v nix-build)" ]; then
  echo 'Error: nix is not installed.' >&2
  # shellcheck disable=SC2016
  echo 'Run `curl https://nixos.org/nix/install | sh` and re-run this script' >&2
  exit 1
fi

GITREV=$(git rev-parse HEAD)

nix-build -A demoClusterDaedalusDev --argstr gitrev "$GITREV" -o "launch_$GITREV"
exec ./launch_"$GITREV"
