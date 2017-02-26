#!/usr/bin/env bash
set -e
set -o pipefail

# Usage:
#   build.sh               build
#   build.sh -t            build and run tests
#   build.sh core          build the core
#   build.sh -c            stack clean
#
# Do `touch .no-nix` if you want builds without Nix.

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
#
# If you want to build core, pass `core`.

args=''
test=false
core=false
clean=false

for var in "$@"
do
  # -t = run tests
  if [[ $var == "-t" ]]; then
    test=true
  # -c = clean
  elif [[ $var == "-c" ]]; then
    clean=true
  # core = build core
  elif [[ $var == "core" ]]; then
    core=true
  # otherwise pass the arg to stack
  else
    args="$args $var"
  fi
done

# TODO: how can --ghc-options be moved into commonargs?
commonargs='--test --no-haddock-deps --bench --jobs=4'
norun='--no-run-tests --no-run-benchmarks'
webwallet='--flag cardano-sl:with-web --flag cardano-sl:with-wallet'

if [ -e .no-nix ]; then
  commonargs="$commonargs --no-nix"
fi

xperl='$|++; s/(.*) Compiling\s([^\s]+)\s+\(\s+([^\/]+).*/\1 \2/p'
xgrep="(^.*warning.*$|^.*error.*$|^    .*$|^.*can't find source.*$|^Module imports form a cycle.*$|^  which imports.*$)|"

if [[ $clean == true ]]; then
  stack clean cardano-sl cardano-sl-core cardano-sl-db
  exit
fi

stack build --ghc-options="+RTS -A256m -n2m -RTS" $commonargs $norun --dependencies-only $args cardano-sl-core
stack build --ghc-options="+RTS -A256m -n2m -RTS" $commonargs $norun --fast $args 2>&1 cardano-sl-core | perl -pe "$xperl" | { grep -E --color "$xgrep" || true; }

stack build --ghc-options="+RTS -A256m -n2m -RTS" $commonargs $norun --dependencies-only $args cardano-sl-db
stack build --ghc-options="+RTS -A256m -n2m -RTS" $commonargs $norun --fast $args 2>&1 cardano-sl-db | perl -pe "$xperl" | { grep -E --color "$xgrep" || true; }

if [[ $core == false ]]; then
  stack build --ghc-options="+RTS -A256m -n2m -RTS" $commonargs $norun $webwallet --dependencies-only cardano-sl $args
  stack build --ghc-options="+RTS -A256m -n2m -RTS" $commonargs $norun $webwallet --fast $args cardano-sl 2>&1 | perl -pe "$xperl" | { grep -E --color "$xgrep" || true; }
fi

if [[ $test == true ]]; then
  stack build --ghc-options="+RTS -A256m -n2m -RTS" $commonargs $webwallet --no-run-benchmarks --fast $args cardano-sl
fi
