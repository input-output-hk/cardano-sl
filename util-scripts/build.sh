#!/usr/bin/env bash
set -e
set -o pipefail

# Usage:
#   build.sh                           build
#   build.sh -t                        build and run tests
#   build.sh core|update|infra         build only a specific project
#   build.sh -c                        stack clean
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

projects="core update infra"

args=''

test=false
clean=false

spec_prj=''

for prj in $projects
do
  if [[ "$@" == "$prj" ]]; then
    spec_prj=$prj
  fi
done

if [[ $spec_prj == "" ]]; then
  for var in "$@"
  do
    # -t = run tests
    if [[ $var == "-t" ]]; then
      test=true
    # -c = clean
    elif [[ $var == "-c" ]]; then
      clean=true
    # otherwise pass the arg to stack
    else
      args="$args $var"
    fi
  done
fi

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
  echo "Cleaning cardano-sl"
  stack clean cardano-sl
  for prj in $projects; do
    echo "Cleaning cardano-sl-$prj"
    stack clean "cardano-sl-$prj"
  done
  exit
fi

to_build=''
if [[ $spec_prj == "" ]]; then
  for prj in $projects; do
    to_build="$to_build cardano-sl-$prj"
  done
  to_build="$to_build cardano-sl"
else
  to_build="cardano-sl-$spec_prj"
fi

echo "Going to build: $to_build"

for prj in $to_build; do
  echo "Building $prj"
  stack build                               \
      --ghc-options="+RTS -A256m -n2m -RTS" \
      $commonargs $webwallet $norun         \
      --dependencies-only                   \
      $args                                 \
      $prj
  stack build                               \
      --ghc-options="+RTS -A256m -n2m -RTS" \
      $commonargs $webwallet $norun         \
      --fast                                \
      $args                                 \
      $prj                                  \
      2>&1                                  \
    | perl -pe "$xperl"                     \
    | { grep -E --color "$xgrep" || true; }
done

if [[ $test == true ]]; then
  stack build                               \
      --ghc-options="+RTS -A256m -n2m -RTS" \
      $commonargs $webwallet                \
      --no-run-benchmarks                   \
      --fast                                \
      $args                                 \
      cardano-sl
fi
