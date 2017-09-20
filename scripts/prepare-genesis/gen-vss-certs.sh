
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

pushd "$REPO_PATH"
for i in $ALL_NODES; do
    stack exec --nix -- cardano-keygen generate-vss --path "${DELEGATE}${i}.key" --system-start "$START_TIME"
done
popd
