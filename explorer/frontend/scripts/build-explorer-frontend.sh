rm -rf .psci_modules/ .pulp-cache/ node_modules/ bower_components/ output/
yarn install
./scripts/generate-explorer-lenses.sh
yarn build:prod
echo "Done generating explorer purescript frontend."
