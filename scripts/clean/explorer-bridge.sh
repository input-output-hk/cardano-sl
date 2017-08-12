#!/usr/bin/env bash
set -e
set -o pipefail

echo "Cleaning Explorer Bridge artifacts..."

cd explorer/frontend
rm -rf .psci_modules/ .pulp-cache/ node_modules/ bower_components/ output/
