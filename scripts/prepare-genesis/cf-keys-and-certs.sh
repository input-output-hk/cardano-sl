
#!/usr/bin/env bash
set -ex

REPO_URL="https://github.com/input-output-hk/cardano-sl.git"
REPO_PATH="$HOME/cardano-sl"
BRANCH="avieth/csl1617_config"

START_TIME="1505930400"    # 18:00 UTC on 2017-09-20
AVVM_SEED_COUNT="3"

IOHK_NODES=(0 1 2)
SGG_NODES=(3 4)
CF_NODES=(5 6)
ALL_NODES="${IOHK_NODES[*]} ${SGG_NODES[*]} ${CF_NODES[*]}"

KEY_PATH="${REPO_PATH}/keys"
DELEGATE="${KEY_PATH}/delegate"
STAKEHOLDER="${KEY_PATH}/stakeholder"
NODES="${CF_NODES[*]}"

echo "Generating stakeholder keys..."

pushd "$REPO_PATH"
for i in $NODES; do
    stack exec --nix -- cardano-keygen generate-key --path "${STAKEHOLDER}${i}.key" --system-start "$START_TIME"
    stack exec --nix -- cardano-keygen read-key --path "${STAKEHOLDER}${i}.key" --system-start "$START_TIME"
done
popd
echo "Generating delegation certificates..."

pushd "$REPO_PATH"
for i in $NODES; do
    stack exec --nix -- cardano-keygen --configuration-file node/configuration.mainnet.yaml --configuration-key testnet_public_full --system-start 0 rearrange --mask "${STAKEHOLDER}${i}.key"

    pushd auxx
    stack exec --nix -- cardano-auxx --configuration-file ../node/configuration.mainnet.yaml --configuration-key testnet_public_full --system-start "$START_TIME" cmd --commands "add-key ${STAKEHOLDER}${i}.key"
    popd
done

pushd auxx
stack exec --nix -- cardano-auxx --configuration-file ../node/configuration.mainnet.yaml --configuration-key testnet_public_full --system-start "$START_TIME" cmd --commands 'listaddr'
popd

skn=0
for i in $NODES; do
    dpk=$(stack exec --nix -- cardano-keygen read-key --path "${DELEGATE}${i}.key" --system-start "$START_TIME" | grep "Primary:" | cut -d" " -f3 | sed "s/;//")
    echo "Extracted delegate public key $i: '${dpk}'"

    pushd auxx
    stack exec --nix -- cardano-auxx --configuration-file ../node/configuration.mainnet.yaml --configuration-key testnet_public_full --system-start "$START_TIME" cmd --commands "delegate-heavy $skn $dpk 0 dump"
    popd

    skn=$((skn+1))
done
popd
