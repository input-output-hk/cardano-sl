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
#   sl                              cardano-sl
#   sl+                             cardano-sl and everything dependent on it

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

# We can't have auxx, node, wallet or explorer here, because they depend on 'cardano-sl'.
projects="util core db lrc infra update ssc txp"

args=''

test=false
coverage=false
clean=false

spec_prj=''

no_nix=false
ram=false
prodMode=
no_code=false
werror=false
for_installer=false
asserts=true
bench_mode=false
no_fast=false

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
    # • --flag cardano-sl-core:-asserts ($asserts)
    # • compiler optimizations ($no_fast)
    bench_mode=true
    no_fast=true
    asserts=false
  # project name = build only the project
  elif [[ $var == "sl" ]] || [[ $var == "sl+" ]] || [[ $var == "all" ]]; then
    spec_prj="all"
  elif [[ $var == "lib" ]]; then
    spec_prj="lib"
  elif [[ $var == "auxx" ]]; then
    spec_prj="auxx"
  elif [[ $var == "wallet" ]]; then
    spec_prj="wallet"
  elif [[ $var == "wallet-new" ]]; then
    spec_prj="wallet-new"
  elif [[ $var == "explorer" ]]; then
    spec_prj="explorer"
  elif [[ $var == "node" ]]; then
    spec_prj="node"
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

if [[ $for_installer == true ]]; then
  commonargs="$commonargs --flag cardano-sl-tools:for-installer"
fi

if [[ $asserts == false ]]; then
  commonargs="$commonargs --flag cardano-sl-core:-asserts"
fi


if [[ $no_fast == true ]]; then
  fast=""
else
  fast="--fast"
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
xsed='1,11d' # the hack is necessary due to warning produced by servant-quickcheck

if [[ $clean == true ]]; then

  echo "Cleaning cardano-sl-tools"
  stack clean cardano-sl-tools

  echo "Cleaning cardano-sl-auxx"
  stack clean cardano-sl-auxx

  echo "Cleaning cardano-sl-wallet"
  stack clean cardano-sl-wallet

  echo "Cleaning cardano-sl-wallet-new"
  stack clean cardano-sl-wallet-new

  echo "Cleaning cardano-sl-explorer"
  stack clean cardano-sl-explorer

  echo "Cleaning cardano-sl-node"
  stack clean cardano-sl-node

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

  to_build="$to_build cardano-sl cardano-sl-auxx cardano-sl-tools cardano-sl-wallet cardano-sl-wallet-new cardano-sl-explorer cardano-sl-node"

elif [[ $spec_prj == "lib" ]]; then
  to_build="cardano-sl"
elif [[ $spec_prj == "node" ]]; then
  to_build="cardano-sl-node"
elif [[ $spec_prj == "auxx" ]]; then
  to_build="cardano-sl-auxx"
elif [[ $spec_prj == "wallet" ]]; then
  to_build="cardano-sl-node cardano-sl-wallet"
elif [[ $spec_prj == "wallet-new" ]]; then
  to_build="cardano-sl-node cardano-sl-wallet-new"
elif [[ $spec_prj == "explorer" ]]; then
  to_build="cardano-sl-node cardano-sl-explorer"
elif [[ $spec_prj == "all" ]]; then
  to_build="" # build everything concurrently
else
  to_build="cardano-sl-$spec_prj"
fi

if [[ $to_build == "" ]]; then
  echo "Going to build: everything, concurrently"
else
  echo "Going to build: $to_build"
fi

for prj in $to_build; do

  echo -e "Building $prj\n"

  # Building deps
  sbuild="stack build --ghc-options=\"$ghc_opts\" $commonargs $norun --dependencies-only $args $prj"
  echo -e "$sbuild\n"
  eval $sbuild 2>&1                         \
    | sed -e "$xsed"

  if [[ $no_code == true ]]; then
    ghc_opts_2="$ghc_opts -fwrite-interface -fno-code"
  else
    ghc_opts_2="$ghc_opts"
  fi

  sbuild="stack build --ghc-options=\"$ghc_opts\" $commonargs $norun $fast $args $prj"
  echo -e "$sbuild\n"

  eval $sbuild 2>&1                         \
    | sed -e "$xsed"                        \
    | perl -pe "$xperl"                     \
    | { grep -E --color "$xgrep" || true; }
done

if [[ $to_build == "" ]]; then
  sbuild="stack build --ghc-options=\"$ghc_opts\" $commonargs $norun $fast $args"
  echo -e "$sbuild\n"
  eval $sbuild 2>&1                         \
    | sed -e "$xsed"                        \
    | perl -pe "$xperl"                     \
    | { grep -E --color "$xgrep" || true; }
fi

if [[ $test == true ]]; then
  stack build                               \
      --ghc-options="$ghc_opts"             \
      $commonargs                           \
      --no-run-benchmarks                   \
      $fast                                 \
      $args                                 \
      cardano-sl                            \
    | sed "$xsed"
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
