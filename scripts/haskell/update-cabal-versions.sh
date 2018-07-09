#!/usr/bin/env bash

if [ -z "$1" ]; then
    echo "Supply single argument -- version to update CSL to"
    exit
fi

testSED=$(sed --version &> /dev/null && echo YES || echo NO)
if [ "x${testSED}" != "xYES" ]; then
  echo "don't know if your version of 'sed' works."
  echo "  on a mac:  install 'gnu-sed' from homebrew/.."
  exit 1
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
