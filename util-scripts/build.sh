#!/usr/bin/env bash
set -e
set -o pipefail

# Usage:
#   build.sh                           build
#   build.sh -t                        build and run tests
#   build.sh core|db|update|infra|sl   build only a specific project
#   build.sh -c                        stack clean
#
# * Do `touch .no-nix` if you want builds without Nix,
#     or pass the --no-nix flag.
# * Do `touch .ram` if you have lots of RAM and want to make builds faster,
#     or pass the --ram flag.

# This script builds the project in a way that is convenient for developers.
# Specifically, it does the following:
#
#   * Builds dependencies without --fast (because Stack might break otherwise)
#   * Builds the project with --fast (to make compilation faster),
#     tests and benchmarks
#   * Highlights error messages in GHC output
#   * Strips unneeded info from GHC output (such as file names)

projects="core db lrc infra update"

args=''

test=false
clean=false

spec_prj=''

no_nix=false
ram=false

if [ -e .no-nix ]; then
  no_nix=true
fi
if [ -e .ram ]; then
  ram=true
fi

for var in "$@"
do
  # -t = run tests
  if [[ $var == "-t" ]]; then
    test=true
  # -c = clean
  elif [[ $var == "-c" ]]; then
    clean=true
  # --no-nix = don't use Nix
  elif [[ $var == "--no-nix" ]]; then
    no_nix=true
  # --ram = use more RAM
  elif [[ $var == "--ram" ]]; then
    ram=true
  # project name = build only the project
  elif [[ $var == "sl" ]]; then
    spec_prj="sl"
  elif [[ " $projects " =~ " $var " ]]; then
    spec_prj=$var
  # otherwise pass the arg to stack
  else
    args="$args $var"
  fi
done

commonargs='--test --no-haddock-deps --bench --jobs=4'
norun='--no-run-tests --no-run-benchmarks'

if [[ $no_nix == true ]]; then
  commonargs="$commonargs --no-nix"
fi

if [[ $ram == true ]]; then
  ghc_opts="-Wwarn +RTS -A2G -n4m -RTS"
else
  ghc_opts="-Wwarn +RTS -A256m -n2m -RTS"
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
elif [[ $spec_prj == "sl" ]]; then
  to_build="cardano-sl"
else
  to_build="cardano-sl-$spec_prj"
fi

echo "Going to build: $to_build"

for prj in $to_build; do
  echo "Building $prj"
  stack build                               \
      --ghc-options="$ghc_opts"             \
      $commonargs $norun                    \
      --dependencies-only                   \
      $args                                 \
      $prj
  stack build                               \
      --ghc-options="$ghc_opts"             \
      $commonargs $norun                    \
      --fast                                \
      $args                                 \
      $prj                                  \
      2>&1                                  \
    | perl -pe "$xperl"                     \
    | { grep -E --color "$xgrep" || true; }
done

if [[ $test == true ]]; then
  stack build                               \
      --ghc-options="$ghc_opts"             \
      $commonargs                           \
      --no-run-benchmarks                   \
      --fast                                \
      $args                                 \
      cardano-sl
fi
