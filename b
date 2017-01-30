#!/usr/bin/env bash
set -e

stack build --ghc-options="+RTS -A256m -n2m -RTS" --test --no-run-tests --no-haddock-deps --bench --no-run-benchmarks --jobs=4 --flag cardano-sl:with-web --flag cardano-sl:with-wallet --dependencies-only

stack build --fast --ghc-options="+RTS -A256m -n2m -RTS" --test --no-run-tests --no-haddock-deps --bench --no-run-benchmarks --jobs=4 --flag cardano-sl:with-web --flag cardano-sl:with-wallet 2>&1 | perl -pe '$|++; s/(.*) Compiling\s([^\s]+)\s+\(\s+([^\/]+).*/\1 \2/p' | grep -E --color "(^.*warning.*$|^.*error.*$|^    .*$)|"

if [[ $1 == "t" ]]; then
  stack build --fast --ghc-options="+RTS -A256m -n2m -RTS" --test --no-haddock-deps --bench --no-run-benchmarks --jobs=4 --flag cardano-sl:with-web --flag cardano-sl:with-wallet
fi
