#!/usr/bin/env bash
set -ex

IOHK_NODES=(0 1 2)
CGG_NODES=(3 4)
CF_NODES=(5 6)
ALL_NODES=("${IOHK_NODES[@]}" "${CGG_NODES[@]}" "${CF_NODES[@]}")

CONF_PARAMS="--configuration-file lib/configuration.yaml --configuration-key mainnet_launch_base"

case "$STAKEHOLDER_NAME" in
  iohk)
    NODES=("${IOHK_NODES[@]}")
    ;;
  cgg)
    NODES=("${CGG_NODES[@]}")
    ;;
  cf)
    NODES=("${CF_NODES[@]}")
    ;;
  *)
    echo "Unknown stakeholder STAKEHOLDER_NAME=$STAKEHOLDER_NAME"
    exit 2
    ;;
esac

DPKS=()

if [[ ! -f "$DELEGATE_PUBS" ]]; then
    echo "Wrong DELEGATE_PUBS passed: \"$DELEGATE_PUBS\" is not an existing file"
    exit 2
fi

while read dpk; do
  if [[ "$dpk" == "" ]];then
      echo "Empty line on \"$DELEGATE_PUBS\""
      exit 2
  fi
  DPKS+=( $dpk )
done < "$DELEGATE_PUBS"
nLen=${#ALL_NODES[@]}
dLen=${#DPKS[@]}
if [[ "$nLen" != "$dLen" ]]; then
  echo "Amount of nodes $nLen not eual to amount of delegate addresses $dLen"
  exit 2
fi

echo "$DPKS"

if [[ "$REPO_PATH" == "" ]]; then
    echo "No REPO_PATH passed"
    exit 2
fi

if [[ "${STAKEHOLDER_SECRET:0:1}" != "/" ]] || [[ "$STAKEHOLDER_SECRET" == "" ]] || [[ -d "$STAKEHOLDER_SECRET" ]]; then
    echo "Folder STAKEHOLDER_SECRET=\"$STAKEHOLDER_SECRET\" already exists or not absolute path passed or no parameter passed"
    exit 1
fi
mkdir -p "$STAKEHOLDER_SECRET"
if [[ ! -d "$STAKEHOLDER_SECRET" ]]; then
  echo "Failed to create STAKEHOLDER_SECRET=\"$STAKEHOLDER_SECRET\""
  exit 1
fi

if [[ "${STAKEHOLDER_PUBLIC:0:1}" != "/" ]] || [[ "$STAKEHOLDER_PUBLIC" == "" ]] || [[ -d "$STAKEHOLDER_PUBLIC" ]]; then
    echo "Folder STAKEHOLDER_PUBLIC=\"$STAKEHOLDER_PUBLIC\" already exists or not absolute path passed or no parameter passed"
    exit 1
fi
mkdir -p "$STAKEHOLDER_PUBLIC"
if [[ ! -d "$STAKEHOLDER_PUBLIC" ]]; then
  echo "Failed to create STAKEHOLDER_PUBLIC=\"$STAKEHOLDER_PUBLIC\""
  exit 1
fi

echo "Generating stakeholder keys..."

pushd "$REPO_PATH"
time for i in ${NODES[@]}; do
    key_path="${STAKEHOLDER_SECRET}/secret${i}.key"
    stack exec --nix -- cardano-keygen $CONF_PARAMS generate-key --path "$key_path"
    if [[ ! -f "$key_path" ]]; then
        echo "File $key_path not created"
        exit 1
    fi
    echo "Created key: "
    stack exec --nix -- cardano-keygen $CONF_PARAMS read-key --path "$key_path"
    stack exec --nix -- cardano-keygen $CONF_PARAMS read-key --path "$key_path" \
        | grep -oE 'address hash: \S+' | sed 's/address hash: //g' >> "$STAKEHOLDER_PUBLIC/pubs.txt"
done
popd
echo "Generating delegation certificates..."

KEYFILE_PATH="${STAKEHOLDER_SECRET}/keyfile"

pushd "$REPO_PATH"

for i in ${NODES[@]}; do
    key_path="${STAKEHOLDER_SECRET}/secret${i}.key"
    stack exec --nix -- cardano-keygen $CONF_PARAMS rearrange --mask "$key_path"
    stack exec --nix -- cardano-auxx --keyfile "$KEYFILE_PATH" $CONF_PARAMS cmd --commands "add-key $key_path"
done

stack exec --nix -- cardano-auxx --keyfile "$KEYFILE_PATH" $CONF_PARAMS cmd --commands 'listaddr'
nAddrs=$(stack exec --nix -- cardano-auxx --keyfile "$KEYFILE_PATH" $CONF_PARAMS cmd --commands 'listaddr' | grep 'addr: ' | wc -l)

nLen=${#NODES[@]}
if [[ "$nLen" != "$nAddrs" ]]; then
  echo "$nAddrs addresses in key storage instead of $nLen expected"
  exit 1
fi

dpks_out="$STAKEHOLDER_PUBLIC/dpks.json"

echo '{' > "$dpks_out"

skn=0
for i in ${NODES[@]}; do
    if [[ $skn -gt 0 ]];then
        echo ',' >> "$dpks_out"
    fi
    dpk="${DPKS[$i]}"
    dump_line=$(stack exec --nix -- cardano-auxx --keyfile "$KEYFILE_PATH" $CONF_PARAMS cmd --commands "delegate-heavy $skn $dpk 0 dump" | grep -E 'JSON: key \S+, value')
    key=$(echo "$dump_line" | sed -r 's/^.*key (\S+),.*$/\1/')
    value=$(echo "$dump_line" | sed -r 's/^.*value (.*)$/\1/')
    if [[ "$key" == "" ]] || [[ "$value" == "" ]]; then 
      echo "Failed to create dp for node $i: key=\"$key\" value=\"$value\""
      exit 1
    fi
    echo -n "\"$key\":$value" >> "$dpks_out"
    skn=$((skn+1))
done
echo '' >> "$dpks_out"
echo '}' >> "$dpks_out"

popd
