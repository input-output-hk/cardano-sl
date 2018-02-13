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
#   scripts/build/cardano-sl.sh core|db|...     build only a specific project (see below)
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
#   lib                             cardano-sl
#   sl | all                        all packages at once

# CUSTOMIZATIONS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# * Pass --no-nix or do `touch .no-nix` if you want builds without Nix.
# * Pass --ram or do `touch .ram`. if you have lots of RAM and want to
#   make builds faster.
# * Pass --limit-ram-* if you want to limit memory used during building.
#   Supported values: --limit-ram-2G, --limit-ram-4G, --limit-ram-6G.
# * Pass -Werror or do `touch .Werror` if you want to compile with -Werror.
# * Pass --for-installer to enable 'for-installer' flag (which means that most
#   of executables won't be built).
# * Pass --no-asserts to disable asserts.
# * Pass --bench-mode to use the configuration used by modern benchmarks.

# Note: this list should be topologically sorted.
projects="networking binary util crypto core db lrc infra ssc txp update delegation block lib node client generator auxx tools explorer wallet wallet-new"

# Returns name of a stack project to build, given the alias.
function pkgNameToProject {
  if [[ $1 == "lib" ]]; then
    echo "cardano-sl"
  else
    echo "cardano-sl-$1"
  fi
}

args=''

test=false
coverage=false
clean=false

spec_prj=''

no_nix=false
ram=false
limit_ram_2G=false
limit_ram_4G=false
limit_ram_6G=false
no_code=false
werror=false
for_installer=false
asserts=true
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
  elif [[ $var == "--limit-ram-2G" ]]; then
    limit_ram_2G=true
  elif [[ $var == "--limit-ram-4G" ]]; then
    limit_ram_4G=true
  elif [[ $var == "--limit-ram-6G" ]]; then
    limit_ram_6G=true
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
    no_fast=true
    asserts=false
  # all = build all at once
  # package name = build only the package
  elif [[ $var == "sl" ]] || [[ $var == "all" ]]; then
    spec_prj="all"
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
  else
    if   [[ $limit_ram_2G == true ]]; then ghc_opts="$ghc_opts +RTS -A256m -n2m -M2G -RTS"
    elif [[ $limit_ram_4G == true ]]; then ghc_opts="$ghc_opts +RTS -A256m -n2m -M4G -RTS"
    elif [[ $limit_ram_6G == true ]]; then ghc_opts="$ghc_opts +RTS -A256m -n2m -M6G -RTS"
    fi
fi

# prettify output of stack build
xperl_pretty='$|++; s/(.*) Compiling\s([^\s]+)\s+\(\s+([^\/]+).*/\1 \2/p'
# more stuff can added to `xperl', e. g. "$xperl_pretty ; $xperl_workaround"
xperl="$xperl_pretty"
xgrep="((^.*warning.*$|^.*error.*$|^    .*$|^.*can't find source.*$|^Module imports form a cycle.*$|^  which imports.*$)|^)"

function cleanPackage () { echo "Cleaning $1"; stack clean $1 ; };
if [[ $clean == true ]]; then
  for prj in $projects; do
    cleanPackage "$(pkgNameToProject $prj)"
  done
  exit
fi

to_build=''
if [[ $spec_prj == "" ]]; then
  for prj in $projects; do
    pkgName=$(pkgNameToProject $prj)
    to_build="$to_build $pkgName"
  done
elif [[ $spec_prj == "wallet" ]]; then
  to_build="cardano-sl-node cardano-sl-wallet"
elif [[ $spec_prj == "wallet-new" ]]; then
  to_build="cardano-sl-node cardano-sl-wallet-new"
elif [[ $spec_prj == "explorer" ]]; then
  to_build="cardano-sl-node cardano-sl-explorer"
elif [[ $spec_prj == "all" ]]; then
  to_build="" # build everything concurrently
else
  to_build=$(pkgNameToProject $spec_prj)
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
  eval $sbuild 2>&1

  if [[ $no_code == true ]]; then
    ghc_opts_2="$ghc_opts -fwrite-interface -fno-code"
  else
    ghc_opts_2="$ghc_opts"
  fi

  sbuild="stack build --ghc-options=\"$ghc_opts\" $commonargs $norun $fast $args $prj"
  echo -e "$sbuild\n"

  eval $sbuild 2>&1                         \
    | perl -pe "$xperl"                     \
    | { grep -E --color "$xgrep" || true; }
done

if [[ $to_build == "" ]]; then
  sbuild="stack build --ghc-options=\"$ghc_opts\" $commonargs $norun $fast $args"
  echo -e "$sbuild\n"
  eval $sbuild 2>&1                         \
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
