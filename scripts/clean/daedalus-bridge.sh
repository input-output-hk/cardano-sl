#!/usr/bin/env bash
set -e
set -o pipefail

echo "Cleaning Daedalus Bridge artifacts..."

cd daedalus
rm -rf .psci_modules/ .pulp-cache/ node_modules/ bower_components/ output/
