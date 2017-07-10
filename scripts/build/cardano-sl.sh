#!/usr/bin/env bash

# This script is based on scripts/build/cardano-sl.sh from cardano-sl project. It probably does some extra not needed work for explorer project. TODO: optimize, remove extra which is not needed in explorer
set -e
set -o pipefail

# DESCRIPTION
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This script builds the project in a way that is convenient for developers.
# It passes the right flags into right places, builds the project with --fast,
# tidies up and highlights error messages in GHC output.

# USAGE
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   scripts/build/cardano-sl.sh                 build
#   scripts/build/cardano-sl.sh -t              build and run tests
#   scripts/build/cardano-sl.sh core|db|...|sl  build only a specific project
#   scripts/build/cardano-sl.sh -k              typecheck but do not build
#   scripts/build/cardano-sl.sh -c              do stack clean
#
# Consider symlinking the script as `b` into the cardano-sl folder because 
# typing `scripts/build/cardano-sl.sh` is annoying.

# MODES
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   Mode                             Options
#   :
#   dev mode                            <nothing>
#   Testnet staging mode with wallet    --tns
#   Testnet staging mode without wallet --tns --no-wallet
#   Testnet mode with wallet            --tn
#   Testnet mode without wallet         --tn --no-wallet
#   Qanet mode with wallet              --qa
#   Qanet mode without wallet           --qa --no-wallet

# CUSTOMIZATIONS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# * Pass --no-nix or do `touch .no-nix` if you want builds without Nix
# * Pass --ram or do `touch .ram`. if you have lots of RAM and want to
#   make builds faster
# * Pass -Werror or do `touch .Werror` if you want to compile with -Werror

projects="core db lrc infra update ssc godtossing txp"

args=''

test=false
clean=false

spec_prj=''

no_nix=false
ram=false
tn=false
tns=false
qa=false
wallet=true
explorer=true
no_code=false
werror=false

if [ -e .no-nix ]; then
  no_nix=true
fi
if [ -e .ram ]; then
  ram=true
fi
if [ -e .Werror ]; then
  werror=true
fi

prodModesCounter=0

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
  # -Werror = compile with -Werror
  elif [[ $var == "-Werror" ]]; then
    werror=true
  # Production modes
  elif [[ $var == "--tns" ]]; then
    tns=true
    prodModesCounter=$((prodModesCounter+1))
  elif [[ $var == "--tn" ]]; then
    tn=true
    prodModesCounter=$((prodModesCounter+1))
  elif [[ $var == "--qa" ]]; then
    qa=true
    prodModesCounter=$((prodModesCounter+1))
  elif [[ $var == "--prod" ]]; then
    echo "--prod flag is outdated, use one of --qa, --tn, --tns" >&2
    exit 12
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

if [[ $prodModesCounter -gt 1 ]]; then
  echo "More than one of --tns --tn --qa specified" >&2
  exit 23
fi

commonargs='--test --no-haddock-deps --bench --jobs=4'
norun='--no-run-tests --no-run-benchmarks'

if [[ $no_nix == true ]]; then
  commonargs="$commonargs --no-nix"
fi

if [[ $prodModesCounter -gt 0 ]]; then
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
dconfig=dev
if [[ $tns == true ]]; then
  dconfig=testnet_staging
elif [[ $tn == true ]]; then
  dconfig=testnet
elif [[ $qa == true ]]; then
  dconfig=qanet
fi
if [[ $prodModesCounter -gt 0 ]] && [[ $wallet == true ]]; then
  dconfig="${dconfig}_wallet"
fi
ghc_opts="-DCONFIG=$dconfig"

if [[ $no_fast == true ]];
  then fast=""
  else fast="--fast"
fi

if [[ $werror == true ]];
  then ghc_opts="$ghc_opts -Werror"
  else ghc_opts="$ghc_opts -Wwarn"
fi

if [[ $ram == true ]];
  then ghc_opts="$ghc_opts +RTS -A2G -n4m -RTS"
  else ghc_opts="$ghc_opts +RTS -A256m -n2m -RTS"
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
  to_build="$to_build cardano-sl cardano-sl-explorer"
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
      cardano-sl-explorer
fi
