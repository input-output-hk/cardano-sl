#!/usr/bin/env bash

if [ -z "$1" ]; then
    echo "Supply single argument -- version to update CSL to"
    exit
fi

newVersion=$1

function updateVersion() {
  sed -E -i -e "s/^(version\\:\\s+)(.+)/\\1$newVersion/" "$1"
}

updateVersion auxx/cardano-sl-auxx.cabal
updateVersion binary/cardano-sl-binary.cabal
updateVersion binary/test/cardano-sl-binary-test.cabal
updateVersion block/bench/cardano-sl-block-bench.cabal
updateVersion block/cardano-sl-block.cabal
updateVersion block/test/cardano-sl-block-test.cabal
updateVersion client/cardano-sl-client.cabal
updateVersion core/cardano-sl-core.cabal
updateVersion core/test/cardano-sl-core-test.cabal
updateVersion crypto/cardano-sl-crypto.cabal
updateVersion crypto/test/cardano-sl-crypto-test.cabal
updateVersion db/cardano-sl-db.cabal
updateVersion delegation/cardano-sl-delegation.cabal
updateVersion delegation/test/cardano-sl-delegation-test.cabal
updateVersion explorer/cardano-sl-explorer.cabal
updateVersion generator/cardano-sl-generator.cabal
updateVersion infra/cardano-sl-infra.cabal
updateVersion infra/test/cardano-sl-infra-test.cabal
updateVersion lib/cardano-sl.cabal
updateVersion lrc/cardano-sl-lrc.cabal
updateVersion lrc/test/cardano-sl-lrc-test.cabal
updateVersion networking/cardano-sl-networking.cabal
updateVersion node-ipc/cardano-sl-node-ipc.cabal
updateVersion node/cardano-sl-node.cabal
updateVersion sinbin/cardano-sl-sinbin.cabal
updateVersion ssc/cardano-sl-ssc.cabal
updateVersion tools/cardano-sl-tools.cabal
updateVersion txp/cardano-sl-txp.cabal
updateVersion txp/test/cardano-sl-txp-test.cabal
updateVersion update/cardano-sl-update.cabal
updateVersion update/test/cardano-sl-update-test.cabal
updateVersion util/cardano-sl-util.cabal
updateVersion util/test/cardano-sl-util-test.cabal
updateVersion wallet-new/cardano-sl-wallet-new.cabal
updateVersion wallet/cardano-sl-wallet.cabal


echo "Updated to version $newVersion"
