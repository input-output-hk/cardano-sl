#!/usr/bin/env bash

if [ -z "$1" ]; then
    echo "Supply single argument -- version to update CSL to"
    exit
fi

newVersion=$1

function updateVersion() {
  sed -E -i -e "s/^(version\\:\\s+)(.+)/\\1$newVersion/" "$1"
}

for CB in $(git ls-files '*/cardano-*.cabal'); do
  echo "   ${CB}"
  updateVersion "${CB}"
done

echo "Updated to version $newVersion"
