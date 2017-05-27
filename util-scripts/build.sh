#!/usr/bin/env bash
set -e
set -o pipefail

# DESCRIPTION
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This script builds the project in a way that is convenient for developers.
# It passes the right flags into right places, builds the project with --fast,
# tidies up and highlights error messages in GHC output.

# USAGE
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   build.sh                           build
#   build.sh -t                        build and run tests
#   build.sh core|db|...|sl            build only a specific project
#   build.sh -k                        typecheck but do not build
#   build.sh -c                        do stack clean
#
# Consider symlinking the script as `b` into the cardano-sl folder because 
# typing `util-scripts/build.sh` is annoying.

# MODES
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   Mode                             Options
#   :
#   dev mode                         <nothing>
#   prod mode with wallet            --prod
#   prod mode without wallet         --prod --no-wallet

# CUSTOMIZATIONS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# * Pass --no-nix or do `touch .no-nix` if you want builds without Nix
# * Pass --ram or do `touch .ram`. if you have lots of RAM and want to
#   make builds faster

projects="core db lrc infra update ssc godtossing"

args=''

test=false
clean=false

spec_prj=''

no_nix=false
ram=false
prod=false
wallet=true
explorer=false
no_code=false

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
  # -k = -fno-code
  elif [[ $var == "-k" ]]; then
    no_code=true
  # --no-nix = don't use Nix
  elif [[ $var == "--no-nix" ]]; then
    no_nix=true
  # --ram = use more RAM
  elif [[ $var == "--ram" ]]; then
    ram=true
  # --prod = compile in production mode
  elif [[ $var == "--prod" ]]; then
    prod=true
  # --no-wallet = don't build in wallet mode
  elif [[ $var == "--no-wallet" ]]; then
    wallet=false
  # disabling --fast
  elif [[ $var == "--explorer" ]]; then
    explorer=true
  # disabling --fast
  elif [[ $var == "-O2" ]]; then
    no_fast=true
  # project name = build only the project
  # (for “godtossing” we allow “gt” as an alias)
  elif [[ $var == "sl" ]]; then
    spec_prj="sl"
  elif [[ $var == "gt" ]]; then
    spec_prj="godtossing"
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

if [[ $prod == true ]]; then
  commonargs="$commonargs --flag cardano-sl-core:-dev-mode"
  export CSL_SYSTEM_TAG=linux64
fi

if [[ $explorer == true ]]; then
  commonargs="$commonargs --flag cardano-sl:with-explorer"
fi

if [[ $wallet == false ]]; then
  commonargs="$commonargs --flag cardano-sl:-with-wallet"
fi

# CONFIG = dev, prod, or wallet
if [[ $prod == false ]]; then
  ghc_opts="-DCONFIG=dev"
elif [[ $prod == true && $wallet == false ]]; then
  ghc_opts="-DCONFIG=prod"
elif [[ $prod == true && $wallet == true ]]; then
  ghc_opts="-DCONFIG=wallet"
fi

if [[ $no_fast == true ]]; then
  fast=""
else
  fast="--fast"
fi

if [[ $ram == true ]]; then
  ghc_opts="$ghc_opts -Wwarn +RTS -A2G -n4m -RTS"
else
  ghc_opts="$ghc_opts -Wwarn +RTS -A256m -n2m -RTS"
fi

xperl='$|++; s/(.*) Compiling\s([^\s]+)\s+\(\s+([^\/]+).*/\1 \2/p'
xgrep="((^.*warning.*$|^.*error.*$|^    .*$|^.*can't find source.*$|^Module imports form a cycle.*$|^  which imports.*$)|^)"

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
  if [[ $no_code == true ]]; then
    ghc_opts_2="$ghc_opts -fwrite-interface -fno-code"
  else
    ghc_opts_2="$ghc_opts"
  fi
  stack build                               \
      --ghc-options="$ghc_opts_2"           \
      $commonargs $norun                    \
      $fast                                 \
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
      $fast                                 \
      $args                                 \
      cardano-sl
fi
