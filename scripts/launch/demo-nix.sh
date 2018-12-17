#!/usr/bin/env bash


display_help() {
    echo "Usage: $0 [option...]" >&2
    echo
    echo "   -d               run with client auth disabled"
    echo "   -w               enable wallet"
    echo "   -i INT           number of wallets to import (default: 0)"
    echo
    echo "$0 is used to launch a demo cluster with limited parameters."

    echo Example Usage:
    echo "  scripts/launch/demo-nix.sh             Demo Cluster with no wallet running"
    echo "  scripts/launch/demo-nix.sh -d -w -i 5  Demo Cluster with wallet, 5 imported keys and client auth disabled"
  }

RUN_WALLET="false"
NUM_IMPORTED_WALLETS=0
DISABLE_CLIENT_AUTH="false"
ARGS=()

while getopts hdwi: option
do
  case "${option}" in
    d) DISABLE_CLIENT_AUTH="true";;
    w) RUN_WALLET="true";;
    i) NUM_IMPORTED_WALLETS="${OPTARG}"; RUN_WALLET="true";;
    h) display_help; exit 0;;
    *) display_help; exit 1
  esac
done
ARGS+=(--arg runWallet "${RUN_WALLET}" --arg numImportedWallets "${NUM_IMPORTED_WALLETS}")

if ! [ -x "$(command -v nix-build)" ]; then
  echo 'Error: nix is not installed.' >&2
  # shellcheck disable=SC2016
  echo 'Run `curl https://nixos.org/nix/install | sh` and re-run this script' >&2
  exit 1
fi

GITREV=$(git rev-parse HEAD)

nix-build -E "(import ./. {}).callPackage ./scripts/launch/demo-cluster { disableClientAuth = $DISABLE_CLIENT_AUTH; numImportedWallets = $NUM_IMPORTED_WALLETS; runWallet = $RUN_WALLET; }" -o "launch_$GITREV"
exec ./launch_"$GITREV"
