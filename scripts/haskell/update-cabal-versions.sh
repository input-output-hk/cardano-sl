#!/usr/bin/env bash

if [ -z "$1" ]; then
    echo "Supply single argument -- version to update CSL to"
    exit
fi

newVersion=$1

function updateVersion() {
  sed -E -i -e "s/^(version\:\s+)(.+)/\1$newVersion/" $1
}

updateVersion lib/cardano-sl.cabal
updateVersion binary/cardano-sl-binary.cabal
updateVersion util/cardano-sl-util.cabal
updateVersion crypto/cardano-sl-crypto.cabal
updateVersion core/cardano-sl-core.cabal
updateVersion infra/cardano-sl-infra.cabal
updateVersion db/cardano-sl-db.cabal
updateVersion lrc/cardano-sl-lrc.cabal
updateVersion auxx/cardano-sl-auxx.cabal
updateVersion explorer/cardano-sl-explorer.cabal
updateVersion ssc/cardano-sl-ssc.cabal
updateVersion update/cardano-sl-update.cabal
updateVersion tools/cardano-sl-tools.cabal
updateVersion txp/cardano-sl-txp.cabal
updateVersion client/cardano-sl-client.cabal
updateVersion generator/cardano-sl-generator.cabal
updateVersion delegation/cardano-sl-delegation.cabal
updateVersion wallet/cardano-sl-wallet.cabal
updateVersion node/cardano-sl-node.cabal

echo "Updated to version $newVersion"
