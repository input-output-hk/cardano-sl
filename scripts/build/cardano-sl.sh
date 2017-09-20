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
#   scripts/build/cardano-sl.sh                 build all packages one-by-one
#   scripts/build/cardano-sl.sh -t              build and run tests
#   scripts/build/cardano-sl.sh core|db|...|sl  build only a specific project (see below)
#   scripts/build/cardano-sl.sh -k              typecheck but do not build
#   scripts/build/cardano-sl.sh -c              do stack clean
#
# Consider symlinking the script as `b` into the cardano-sl folder because
# typing `scripts/build/cardano-sl.sh` is annoying.

# PROJECTS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   Project argument                Package name
#   :
#   core, db, etc.                  cardano-sl-{core,db,etc.}
#   gt                              cardano-sl-godtossing (just an alias)
#   sl                              cardano-sl
#   sl+                             cardano-sl and everything dependent on it

# MODES
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# NOTE
# You can try building any of these modes, but in some branches some of
# these modes may be unavailable (no genesis).
# See constants.yaml for more information on different compilation modes.
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   Mode                             Options
#   :
#   dev mode                            <nothing>
#   Testnet public mode with wallet     --tnp
#   Testnet public mode without wallet  --tnp --no-wallet
#   Testnet staging mode with wallet    --tns
#   Testnet staging mode without wallet --tns --no-wallet
#   Dev long epoch mode with wallet     --dnl
#   Dev long epoch mode without wallet  --dnl --no-wallet
#   Dev short epoch mode with wallet    --dns
#   Dev short epoch mode without wallet --dns --no-wallet

# CUSTOMIZATIONS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# * Pass --no-nix or do `touch .no-nix` if you want builds without Nix.
# * Pass --ram or do `touch .ram`. if you have lots of RAM and want to
#   make builds faster.
# * Pass -Werror or do `touch .Werror` if you want to compile with -Werror.
# * Pass --for-installer to enable 'for-installer' flag (which means that most
#   of executables won't be built).
# * Pass --no-asserts to disable asserts.
# * Pass --bench-mode to use the configuration used by modern benchmarks.

# We can't have lwallet, wallet or explorer here, because it depends on 'cardano-sl'.
projects="core db lrc infra update ssc godtossing txp"

args=''

test=false
coverage=false
clean=false

spec_prj=''

no_nix=false
ram=false
prodMode=
wallet=true
explorer=true
no_code=false
werror=false
for_installer=false
asserts=true
bench_mode=false

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
  # --coverage = Produce test coverage report
  elif [[ $var == "--coverage" ]]; then
      coverage=true
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
  elif [[ $var == "--tnp" ]]; then
    prodMode="testnet_public"
    prodModesCounter=$((prodModesCounter+1))
  elif [[ $var == "--tns" ]]; then
    prodMode="testnet_staging"
    prodModesCounter=$((prodModesCounter+1))
  elif [[ $var == "--dnl" ]]; then
    prodMode="devnet_longep"
    prodModesCounter=$((prodModesCounter+1))
  elif [[ $var == "--dns" ]]; then
    prodMode="devnet_shortep"
    prodModesCounter=$((prodModesCounter+1))
  elif [[ $var == "--prod" ]]; then
    echo "--prod flag is outdated, see this script documentation, section MODES" >&2
    exit 12
  # --no-wallet = don't build in wallet mode
  elif [[ $var == "--no-wallet" ]]; then
    wallet=false
  # --no-explorer = build without Explorer (support)
  elif [[ $var == "--no-explorer" ]]; then
    explorer=false
  # --for-installer = build with for-installer flag
  elif [[ $var == "--for-installer" ]]; then
    for_installer=true
  # disabling --fast
  elif [[ $var == "-O2" ]]; then
    no_fast=true
  # disabling asserts
  elif [[ $var == "--no-asserts" ]]; then
    asserts=false
  # benchmarks config
  elif [[ $var == "--bench-mode" ]]; then
    # We want:
    # • --flag cardano-sl-core:dev-mode (default)
    # • --flag cardano-sl-core:-asserts ($asserts)
    # • compiler optimizations ($no_fast)
    # • disable explorer ($explorer)
    bench_mode=true
    no_fast=true
    asserts=false
    explorer=false
  # project name = build only the project
  # (for “godtossing” we allow “gt” as an alias)
  elif [[ $var == "sl" ]]; then
    spec_prj="sl"
  elif [[ $var == "sl+" ]]; then
    spec_prj="sl+"
  elif [[ $var == "gt" ]]; then
    spec_prj="godtossing"
  elif [[ $var == "lwallet" ]]; then
    spec_prj="lwallet"
  elif [[ $var == "wallet" ]]; then
    spec_prj="wallet"
  elif [[ $var == "explorer" ]]; then
    spec_prj="explorer"
  elif [[ $var == "tools" ]]; then
    spec_prj="tools"
  elif [[ " $projects " =~ " $var " ]]; then
    spec_prj=$var
  # otherwise pass the arg to stack
  else
    args="$args $var"
  fi
done

if [[ $prodModesCounter -gt 1 ]]; then
  echo "More than one build mode specified" >&2
  exit 23
fi

if [[ $prodModesCounter -gt 0 ]]; then
  if [[ $bench_mode == true ]]; then
    echo "Prod mode can't be used with bench mode" >&2
    exit 26
  fi
fi

commonargs='--test --no-haddock-deps --bench --jobs=4'
norun='--no-run-tests --no-run-benchmarks'

if [[ $no_nix == true ]]; then
  commonargs="$commonargs --no-nix"
fi

if [[ "$prodMode" != "" ]]; then
  commonargs="$commonargs --flag cardano-sl-core:-dev-mode"
  export CSL_SYSTEM_TAG=linux64
fi

if [[ $explorer == false ]]; then
  commonargs="$commonargs --flag cardano-sl:-with-explorer"
fi

if [[ $wallet == true ]]; then
  commonargs="$commonargs --flag cardano-sl:with-wallet"
fi

if [[ $for_installer == true ]]; then
  commonargs="$commonargs --flag cardano-sl-tools:for-installer"
fi

if [[ $asserts == false ]]; then
  commonargs="$commonargs --flag cardano-sl-core:-asserts"
fi

# CONFIG
if [[ $bench_mode == true ]]; then
  dconfig=benchmark
else
  dconfig=dev
fi
if [[ "$prodMode" != "" ]]; then
  dconfig=$prodMode
  if [[ $wallet == true ]]; then
    dconfig="${dconfig}_wallet"
  else
    dconfig="${dconfig}_full"
  fi
fi
ghc_opts="-DGITREV=`git rev-parse HEAD`"

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

  echo "Cleaning cardano-sl-tools"
  stack clean cardano-sl-tools

  echo "Cleaning cardano-sl-lwallet"
  stack clean cardano-sl-lwallet

  echo "Cleaning cardano-sl-wallet"
  stack clean cardano-sl-wallet

  echo "Cleaning cardano-sl-explorer"
  stack clean cardano-sl-explorer

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

  to_build="$to_build cardano-sl cardano-sl-lwallet cardano-sl-tools cardano-sl-wallet cardano-sl-explorer"

elif [[ $spec_prj == "sl" ]]; then
  to_build="cardano-sl"
elif [[ $spec_prj == "lwallet" ]]; then
  to_build="cardano-sl-lwallet"
elif [[ $spec_prj == "wallet" ]]; then
  to_build="cardano-sl-wallet"
elif [[ $spec_prj == "explorer" ]]; then
  to_build="cardano-sl-explorer"
elif [[ $spec_prj == "sl+" ]]; then
  to_build="cardano-sl cardano-sl-lwallet cardano-sl-tools cardano-sl-explorer cardano-sl-wallet "
else
  to_build="cardano-sl-$spec_prj"
fi

# A warning for invalid flag usage when building wallet. This should not happen.
if [[ $to_build == *"wallet"* && $wallet == false ]]; then
  echo "You can't build output with wallet and not use wallet! Invalid flag '--no-wallet'."
  exit
fi

# A warning for invalid flag usage when building explorer. This should not happen.
if [[ $to_build == *"explorer"* && $explorer == false ]]; then
  echo "You can't build output with explorer and not use explorer! Invalid flag '--no-explorer'."
  exit
fi

echo "Going to build: $to_build"
echo "'wallet' flag: $wallet"
echo "'explorer' flag: $explorer"

for prj in $to_build; do

  echo -e "Building $prj\n"
  sbuild="stack build --ghc-options=\"$ghc_opts\" $commonargs $norun --dependencies-only $args $prj"
  echo -e "$sbuild\n"
  eval $sbuild

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

if [[ $coverage == true ]]; then
  stack build                               \
      --ghc-options="$ghc_opts"             \
      $commonargs                           \
      --no-run-benchmarks                   \
      $fast                                 \
      $args                                 \
      --coverage;
  stack hpc report $to_build
fi
