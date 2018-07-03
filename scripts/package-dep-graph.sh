#!/bin/bash -eu

tmpdir=$(mktemp -d "${TMPDIR:-/tmp/}$(basename \"$0\").XXXXXXXXXXXX")

# 'tred' and 'dot' are in the 'graphviz' of most Linux distributions.

prunefiles=$(find . -name \*.cabal -exec basename {} \; \
    | grep -v stack-work | grep test.cabal \
    | sed 's/\.cabal//' | tr '\n' ',')

stack dot --prune "${prunefiles}" > "${tmpdir}/full-dependencies.dot"

tred "${tmpdir}/full-dependencies.dot" > "${tmpdir}/direct-dependencies.dot"

dot -Tpng "${tmpdir}/direct-dependencies.dot" -o cardano-sl-pkg-deps.png

rm -rf "${tmpdir:?}/"

echo "Generated cardano-sl-pkg-deps.png"
