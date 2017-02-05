#!/usr/bin/env bash
set -e
set -o pipefail

# This script builds the project in a way that is convenient for developers.
# Specifically, it does the following:
#
#   * Builds dependencies without --fast (because Stack might break otherwise)
#   * Builds the project with --fast (to make compilation faster),
#     tests, benchmarks, and also sets flags `with-wallet` and `with-web`
#   * Highlights error messages in GHC output
#   * Strips unneeded info from GHC output (such as file names)
#
# To run tests, pass the `-t` argument to the script. Other arguments given
# to the script will be passed to Stack.

args=''
test=false
for var in "$@"
do
  if [[ $var == "-t" ]]; then
    test=true
  else
    args="$args $var"
  fi
done

stack build $args --ghc-options="+RTS -A256m -n2m -RTS" --test --no-run-tests --no-haddock-deps --bench --no-run-benchmarks --jobs=4 --flag cardano-sl:with-web --flag cardano-sl:with-wallet --dependencies-only

stack build $args --fast --ghc-options="+RTS -A256m -n2m -RTS" --test --no-run-tests --no-haddock-deps --bench --no-run-benchmarks --jobs=4 --flag cardano-sl:with-web --flag cardano-sl:with-wallet 2>&1 | perl -pe '$|++; s/(.*) Compiling\s([^\s]+)\s+\(\s+([^\/]+).*/\1 \2/p' | grep -E --color "(^.*warning.*$|^.*error.*$|^    .*$|^Module imports form a cycle.*$|^  which imports.*$)|"

if [[ $test == true ]]; then
  stack build $args --fast --ghc-options="+RTS -A256m -n2m -RTS" --test --no-haddock-deps --bench --no-run-benchmarks --jobs=4 --flag cardano-sl:with-web --flag cardano-sl:with-wallet
fi
