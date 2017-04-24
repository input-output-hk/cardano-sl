#!/bin/sh

if [ -z "$1" ]; then
    echo "Supply single argument -- version to update CSL to"
    exit
fi

newVersion=$1

function updateVersion() {
  sed -E -i -e "s/^(version\:\s+)(.+)/\1$newVersion/" $1 
}

updateVersion cardano-sl.cabal
updateVersion core/cardano-sl-core.cabal
updateVersion infra/cardano-sl-infra.cabal
updateVersion db/cardano-sl-db.cabal
updateVersion lrc/cardano-sl-lrc.cabal

echo "Updated to version $newVersion"
