#!/usr/bin/env bash
set -ex

AVVM_SEED_COUNT=100

if [[ "$REPO_PATH" == "" ]]; then
    echo "No REPO_PATH passed"
    exit 2
fi

if [[ "${AVVM_SECRET:0:1}" != "/" ]] || [[ "$AVVM_SECRET" == "" ]] || [[ -d "$AVVM_SECRET" ]]; then
    echo "Folder AVVM_SECRET=\"$AVVM_SECRET\" already exists or not absolute path passed or no parameter passed"
    exit 1
fi
mkdir -p "$AVVM_SECRET"
if [[ ! -d "$AVVM_SECRET" ]]; then
  echo "Failed to create AVVM_SECRET=\"$AVVM_SECRET\""
  exit 1
fi

if [[ "${AVVM_PUBLIC:0:1}" != "/" ]] || [[ "$AVVM_PUBLIC" == "" ]] || [[ -d "$AVVM_PUBLIC" ]]; then
    echo "Folder AVVM_PUBLIC=\"$AVVM_PUBLIC\" already exists or not absolute path passed or no parameter passed"
    exit 1
fi
mkdir -p "$AVVM_PUBLIC"
if [[ ! -d "$AVVM_PUBLIC" ]]; then
  echo "Failed to create AVVM_PUBLIC=\"$AVVM_PUBLIC\""
  exit 1
fi

pushd "$REPO_PATH"


stack exec --nix -- cardano-keygen --configuration-file node/configuration.yaml --configuration-key mainnet_launch_base generate-avvm-seeds -n "$AVVM_SEED_COUNT" -o "$AVVM_SECRET"

i=$AVVM_SEED_COUNT
{
  echo '{'
  while [[ $i -gt 0 ]]; do
    key_i_pk="$AVVM_SECRET/key$i.pk"
    key_i_sk="$AVVM_SECRET/key$i.seed"
    if [[ ! -f "$key_i_pk" ]] || [[ ! -f "$key_i_sk" ]]; then
        echo "Key $i not generated: either .pk or .sk file missing" >&2
        exit 1
    fi
    key=`cat "$key_i_pk" | sed 's/+/-/g' | sed 's/\//_/g'`
    echo -n "  \"$key\": \"1000000\""
    if [[ $i -gt 1 ]];then
      echo ','
    fi
    rm "$key_i_pk"
    i=$((i-1))
  done
  echo ''
  echo '}'
} > "$AVVM_PUBLIC/utxo.json"


popd
