#!/usr/bin/env bash
set -ex

REPO_URL="https://github.com/input-output-hk/cardano-sl.git"
REPO_PATH="$HOME/cardano-sl"
BRANCH="cardano-sl-1.0"

START_TIME="1505930400"    # 18:00 UTC on 2017-09-20
AVVM_SEED_COUNT="3"

IOHK_NODES=(0 1 2)
SGG_NODES=(3 4)
CF_NODES=(5 6)
ALL_NODES="${IOHK_NODES[*]} ${SGG_NODES[*]} ${CF_NODES[*]}"

KEY_PATH="${REPO_PATH}/keys"
DELEGATE="${KEY_PATH}/delegate"
STAKEHOLDER="${KEY_PATH}/stakeholder"
if which nix-env; then
    echo "Nix package manager already installed."
else
    echo "Installing the nix package manager."
    curl https://nixos.org/nix/install | sh
    export PATH="$PATH:$HOME/.nix-profile/bin"
fi

nix-env -i git stack

if [ -d "$REPO_PATH" ]; then
    echo "ERROR: '$REPO_PATH' already exists; delete and restart."
    exit 1
fi
git clone "$REPO_URL" "$REPO_PATH"
pushd "$REPO_PATH"
git checkout "$BRANCH"
git pull
time stack build --nix --ghc-options="-Wwarn"
popd
