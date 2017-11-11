#!/usr/bin/env bash

# Make sure we're in a tmux session.
if ! [ -n "$TMUX" ]; then
  echo "You must run this script from the tmux session!"
  exit 1
fi

base=$(dirname "$0")
source "$base"/../common-functions.sh

if [[ "$CONFIG_KEY" == "" ]];then
  export CONFIG_KEY=dev
fi

if [[ "$CONFIG" == "" ]];then
  export CONFIG=lib/configuration.yaml
fi

if [[ "$UI" == "" ]];then
  export UI=scripts/launch/ui-simulator.sh
fi

rm -R run; mkdir run

export WALLET_CONFIG=run/configuration.wallet.yaml
export PATH="$(stack path --local-install-root)/bin/:$PATH"

scripts/launch/Test.hs --configuration-key $CONFIG_KEY --configuration-file $CONFIG gen-wallet-conf -o $WALLET_CONFIG || exit 1

scripts/launch/demo-with-wallet-api.sh

echo -n "Enter 'start' to execute update > "
while read l; do
  case "$l" in
    start)
      break
      ;;
    *)
      echo -n "Enter 'start' to execute update > "
      ;;
  esac
done

scripts/launch/Test.hs --configuration-key $CONFIG_KEY --configuration-file $CONFIG update

cd run/serve-upd
webfsd -F -p 10228

echo "Update executed"
