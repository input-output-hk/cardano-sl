#!/bin/bash -eu

################################################################################
# Check deps
################################################################################

if ! command -v dot >/dev/null 2>&1; then
    echo "\`dot\` not found. It is usually in the \`graphviz\` package."
    exit 1
fi

if ! command -v tred >/dev/null 2>&1; then
    echo "\`tred\` not found. It is usually in the \`graphviz\` package."
    exit 1
fi

################################################################################
# Argument processing
################################################################################

script=$(basename "$0")
stack_dot_flags=""
include_test_bench="0"

while getopts "het" opt; do
  case $opt in
    h)
      echo "usage: ./${script} OPTS" >&2
      echo "OPTS:" >&2
      echo "    -h : show help" >&2
      echo "    -e : include external dependencies in graph" >&2
      echo "    -t : include test+bench packages in graph" >&2
      exit 0
      ;;
    e)
      stack_dot_flags="${stack_dot_flags} --external"
      ;;
    t)
      include_test_bench="1"
      ;;
    ?)
      echo "Invalid option: -$OPTARG" >&2
      exit 1
      ;;
  esac
done


################################################################################
# Generate graph
################################################################################

tmpdir=$(mktemp -d "${TMPDIR:-/tmp/}$(basename \"$0\").XXXXXXXXXXXX")
outfile="cardano-sl-pkg-deps.png"

# 'tred' and 'dot' are in the 'graphviz' of most Linux distributions.

if [ "${include_test_bench}" = "0" ]; then
    prunefiles=$(find . -name \*.cabal -exec basename {} \; \
        | grep -v stack-work | grep "test.cabal\|bench.cabal" \
        | sed 's/\.cabal//' | tr '\n' ',')
    stack_dot_flags="${stack_dot_flags} --prune ${prunefiles}"
fi

stack dot ${stack_dot_flags} > "${tmpdir}/full-dependencies.dot"

tred "${tmpdir}/full-dependencies.dot" > "${tmpdir}/direct-dependencies.dot"

dot -Tpng "${tmpdir}/direct-dependencies.dot" -o ${outfile}

rm -rf "${tmpdir:?}/"

echo "Generated ${outfile}"
