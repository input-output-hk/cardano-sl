#!/usr/bin/env bash
set -ex

IOHK_NODES=(0 1 2)
SGG_NODES=(3 4)
CF_NODES=(5 6)
ALL_NODES="${IOHK_NODES[*]} ${SGG_NODES[*]} ${CF_NODES[*]}"

if [[ "$DELEGATE_PUBS" == "" ]] || [[ "${DELEGATE_PUBS:0:1}" != "/" ]] || [[ -f "$DELEGATE_PUBS" ]]; then
    echo "Wrong DELEGATE_PUBS passed: \"$DELEGATE_PUBS\" (empty or file exists or path not absolute)"
    exit 2
fi

if [[ "$REPO_PATH" == "" ]]; then
    echo "No REPO_PATH passed"
    exit 2
fi

if [[ "${DELEGATE_KEY_PATH:0:1}" != "/" ]]; then
    echo "Not absolute path DELEGATE_KEY_PATH passed: \"$DELEGATE_KEY_PATH\""
    exit 2
fi

if [[ "$DELEGATE_KEY_PATH" == "" ]]; then
    echo "No DELEGATE_KEY_PATH passed"
    exit 2
fi

if [[ -d "$DELEGATE_KEY_PATH" ]]; then
    echo "Folder DELEGATE_KEY_PATH=$DELEGATE_KEY_PATH already exists"
    exit 1
fi

mkdir -p "$DELEGATE_KEY_PATH"

echo -n '' > "$DELEGATE_PUBS"

if [[ ! -f "$DELEGATE_PUBS" ]]; then
    echo "File $DELEGATE_PUBS not created"
    exit 1
fi

pushd "$REPO_PATH"
time for i in $ALL_NODES; do
    key_path="${DELEGATE_KEY_PATH}/node${i}.key"
    stack exec --nix -- cardano-keygen --configuration-file node/configuration.mainnet.yaml --configuration-key mainnet_dryrun_base generate-key --path "$key_path"
    if [[ ! -f "$key_path" ]]; then
        echo "File $key_path not created"
        exit 1
    fi
    echo "Created key: "
    stack exec --nix -- cardano-keygen --configuration-file node/configuration.mainnet.yaml --configuration-key mainnet_dryrun_base read-key --path "$key_path"
    stack exec --nix -- cardano-keygen --configuration-file node/configuration.mainnet.yaml --configuration-key mainnet_dryrun_base read-key --path "$key_path" \
        | grep -oE 'address hash: \S+' | sed 's/address hash: //g' >> $DELEGATE_PUBS
done
popd
