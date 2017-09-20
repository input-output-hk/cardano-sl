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
NODES="${SGG_NODES[*]}"

echo "Generating stakeholder keys..."

pushd "$REPO_PATH"
for i in $NODES; do
    stack exec --nix -- cardano-keygen --configuration-file node/configuration.mainnet.yaml --configuration-key mainnet_dryrun_base generate-key --path "${STAKEHOLDER}${i}.key"
    stack exec --nix -- cardano-keygen --configuration-file node/configuration.mainnet.yaml --configuration-key mainnet_dryrun_base read-key --path "${STAKEHOLDER}${i}.key"
done
popd
echo "Generating delegation certificates..."

pushd "$REPO_PATH"
for i in $NODES; do
    stack exec --nix -- cardano-keygen --configuration-file node/configuration.mainnet.yaml --configuration-key mainnet_dryrun_base rearrange --mask "${STAKEHOLDER}${i}.key"

    pushd auxx
    stack exec --nix -- cardano-auxx --configuration-file ../node/configuration.mainnet.yaml --configuration-key mainnet_dryrun_base cmd --commands "add-key ${STAKEHOLDER}${i}.key"
    popd
done

pushd auxx
stack exec --nix -- cardano-auxx --configuration-file ../node/configuration.mainnet.yaml --configuration-key mainnet_dryrun_base cmd --commands 'listaddr'
popd

skn=0
for i in $NODES; do
    dpk=$(stack exec --nix -- cardano-keygen --configuration-file node/configuration.mainnet.yaml --configuration-key mainnet_dryrun_base read-key --path "${DELEGATE}${i}.key" | grep "Primary:" | cut -d" " -f3 | sed "s/;//")
    echo "Extracted delegate public key $i: '${dpk}'"

    pushd auxx
    stack exec --nix -- cardano-auxx --configuration-file ../node/configuration.mainnet.yaml --configuration-key mainnet_dryrun_base cmd --commands "delegate-heavy $skn $dpk 0 dump"
    popd

    skn=$((skn+1))
done
popd
