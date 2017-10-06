#!/usr/bin/env bash
set -ex

IOHK_NODES=(0 1 2)
CGG_NODES=(3 4)
CF_NODES=(5 6)
ALL_NODES="${IOHK_NODES[*]} ${CGG_NODES[*]} ${CF_NODES[*]}"

CONF_PARAMS="--configuration-file lib/configuration.yaml --configuration-key mainnet_launch_base"

if [[ "$REPO_PATH" == "" ]]; then
    echo "No REPO_PATH passed"
    exit 2
fi

if [[ "${DELEGATE_SECRET:0:1}" != "/" ]] || [[ "$DELEGATE_SECRET" == "" ]] || [[ -d "$DELEGATE_SECRET" ]]; then
    echo "Folder DELEGATE_SECRET=\"$DELEGATE_SECRET\" already exists or not absolute path passed or no parameter passed"
    exit 1
fi
mkdir -p "$DELEGATE_SECRET"
if [[ ! -d "$DELEGATE_SECRET" ]]; then
  echo "Failed to create DELEGATE_SECRET=\"$DELEGATE_SECRET\""
  exit 1
fi

if [[ "${DELEGATE_PUBLIC:0:1}" != "/" ]] || [[ "$DELEGATE_PUBLIC" == "" ]] || [[ -d "$DELEGATE_PUBLIC" ]]; then
    echo "Folder DELEGATE_PUBLIC=\"$DELEGATE_PUBLIC\" already exists or not absolute path passed or no parameter passed"
    exit 1
fi
mkdir -p "$DELEGATE_PUBLIC"
if [[ ! -d "$DELEGATE_PUBLIC" ]]; then
  echo "Failed to create DELEGATE_PUBLIC=\"$DELEGATE_PUBLIC\""
  exit 1
fi

vss_out="$DELEGATE_PUBLIC/vss.json"

echo '{' > "$vss_out"

pushd "$REPO_PATH"
skn=0
time for i in $ALL_NODES; do
    key_path="${DELEGATE_SECRET}/node${i}.key"
    stack exec --nix -- cardano-keygen $CONF_PARAMS generate-key --path "$key_path"
    if [[ ! -f "$key_path" ]]; then
        echo "File $key_path not created"
        exit 1
    fi
    echo "Created key: "
    stack exec --nix -- cardano-keygen $CONF_PARAMS read-key --path "$key_path"
    stack exec --nix -- cardano-keygen $CONF_PARAMS read-key --path "$key_path" \
        | grep -oE 'Primary: \S+;' | sed 's/Primary: //g' | sed "s/;//" >> "$DELEGATE_PUBLIC/pubs.txt"

    echo "Generating vss:"

    if [[ $skn -gt 0 ]];then
        echo ',' >> "$vss_out"
    fi
    dump_line=$(stack exec --nix -- cardano-keygen $CONF_PARAMS generate-vss --path "$key_path" | grep -E 'JSON: key \S+, value')
    key=$(echo "$dump_line" | sed -r 's/^.*key (\S+),.*$/\1/')
    value=$(echo "$dump_line" | sed -r 's/^.*value (.*)$/\1/')
    if [[ "$key" == "" ]] || [[ "$value" == "" ]]; then 
      echo "Failed to dump vss for node $i: key=\"$key\" value=\"$value\""
      exit 1
    fi
    echo -n "\"$key\":$value" >> "$vss_out"
    skn=$((skn+1))
done
echo ''  >> "$vss_out"
echo '}' >> "$vss_out"
popd
