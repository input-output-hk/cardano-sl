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

# CUSTOMIZATIONS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# * Pass --no-nix or do `touch .no-nix` if you want builds without Nix.
# * Pass --ram or do `touch .ram`. if you have lots of RAM and want to
#   make builds faster.
# * Pass -Werror or do `touch .Werror` if you want to compile with -Werror.
# * Pass --for-installer to enable 'for-installer' flag (which means that most
#   of executables won't be built).

# We can't have auxx here, because it depends on 'cardano-sl'.
projects="core db lrc infra update ssc godtossing txp"

args=''

test=false
coverage=false
clean=false

spec_prj=''

no_nix=false
ram=false
wallet=true
explorer=false
no_code=false
werror=false
for_installer=false

if [ -e .no-nix ]; then
  no_nix=true
fi
if [ -e .ram ]; then
  ram=true
fi
if [ -e .Werror ]; then
  werror=true
fi

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
  # --no-wallet = don't build in wallet mode
  elif [[ $var == "--no-wallet" ]]; then
    wallet=false
  # --explorer = build with Explorer support
  elif [[ $var == "--explorer" ]]; then
    explorer=true
  # --for-installer = build with for-installer flag
  elif [[ $var == "--for-installer" ]]; then
    for_installer=true
  # disabling --fast
  elif [[ $var == "-O2" ]]; then
    no_fast=true
  # project name = build only the project
  # (for “godtossing” we allow “gt” as an alias)
  elif [[ $var == "sl" ]]; then
    spec_prj="sl"
  elif [[ $var == "sl+" ]]; then
    spec_prj="sl+"
  elif [[ $var == "gt" ]]; then
    spec_prj="godtossing"
  elif [[ $var == "auxx" ]]; then
    spec_prj="auxx"
  elif [[ $var == "tools" ]]; then
    spec_prj="tools"
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

if [[ $explorer == true ]]; then
  commonargs="$commonargs --flag cardano-sl:with-explorer"
fi

if [[ $for_installer == true ]]; then
  commonargs="$commonargs --flag cardano-sl-tools:for-installer"
fi

if [[ $wallet == false ]]; then
  commonargs="$commonargs --flag cardano-sl:-with-wallet"
fi

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
  echo "Cleaning cardano-sl-auxx"
  stack clean cardano-sl-auxx
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
elif [[ $spec_prj == "auxx" ]]; then
  to_build="cardano-sl-auxx"
elif [[ $spec_prj == "sl+" ]]; then
  to_build="cardano-sl cardano-sl-auxx cardano-sl-tools"
else
  to_build="cardano-sl-$spec_prj"
fi

echo "Going to build: $to_build"

for prj in $to_build; do
 
  echo -e "Building $prj\n"
  sbuild="stack build --ghc-options=\"$ghc_opts\" $commonargs $norun --dependencies-only $args $prj"
  echo -e "$sbuild\n\n"
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
      cardano-sl-explorer
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
