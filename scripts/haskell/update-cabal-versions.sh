#!/usr/bin/env bash

if [ -z "$1" ]; then
    echo "Supply single argument -- version to update CSL to"
    exit
fi

newVersion=$1

function updateVersion() {
  sed -E -i -e "s/^(version\:\s+)(.+)/\1$newVersion/" $1
}

updateVersion auxx/cardano-sl-auxx.cabal
updateVersion binary/cardano-sl-binary.cabal
updateVersion block/cardano-sl-block.cabal
updateVersion client/cardano-sl-client.cabal
updateVersion core/cardano-sl-core.cabal
updateVersion crypto/cardano-sl-crypto.cabal
updateVersion db/cardano-sl-db.cabal
updateVersion delegation/cardano-sl-delegation.cabal
updateVersion explorer/cardano-sl-explorer.cabal
updateVersion generator/cardano-sl-generator.cabal
updateVersion infra/cardano-sl-infra.cabal
updateVersion lib/cardano-sl.cabal
updateVersion lrc/cardano-sl-lrc.cabal
updateVersion node/cardano-sl-node.cabal
updateVersion networking/cardano-sl-networking.cabal
updateVersion ssc/cardano-sl-ssc.cabal
updateVersion tools/cardano-sl-tools.cabal
updateVersion txp/cardano-sl-txp.cabal
updateVersion update/cardano-sl-update.cabal
updateVersion util/cardano-sl-util.cabal
updateVersion wallet/cardano-sl-wallet.cabal
updateVersion wallet/cardano-sl-wallet-new.cabal

echo "Updated to version $newVersion"
