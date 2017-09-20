#!/usr/bin/env bash
set -ex

REPO_URL="https://github.com/input-output-hk/cardano-sl.git"
REPO_PATH="$HOME/cardano-sl"
BRANCH="cardano-sl-1.0-launch-tools"

AVVM_SEED_COUNT="3"

IOHK_NODES=(0 1 2)
SGG_NODES=(3 4)
CF_NODES=(5 6)
ALL_NODES="${IOHK_NODES[*]} ${SGG_NODES[*]} ${CF_NODES[*]}"

KEY_PATH="${REPO_PATH}/keys"
DELEGATE="${KEY_PATH}/delegate"
STAKEHOLDER="${KEY_PATH}/stakeholder"

pushd "$REPO_PATH"
rm -rf keys && mkdir -p keys
time for i in $ALL_NODES; do
    stack exec --nix -- cardano-keygen --configuration-file node/configuration.mainnet.yaml --configuration-key mainnet_dryrun_base generate-key --path "${DELEGATE}${i}.key"
    stack exec --nix -- cardano-keygen --configuration-file node/configuration.mainnet.yaml --configuration-key mainnet_dryrun_base read-key --path "${DELEGATE}${i}.key"
done
popd
