#!/bin/bash
set -euo pipefail

echo "Cardano SL Wallet Web API updating"

readonly CARDANO_DOCS_REPO="${HOME}"/cardanodocs
readonly SWAGGER_WALLET_API_JSON_SPEC=wallet-web-api-swagger.json
readonly WALLET_API_PRODUCED_ROOT=wallet-web-api
readonly WALLET_API_HTML=index.html
readonly WALLET_API_ROOT=technical/wallet/api

echo "**** 1. Get Swagger-specification for wallet web API ****"
stack exec --nix -- cardano-swagger
# Done, 'SWAGGER_WALLET_API_JSON_SPEC' file is already here.

echo "**** 2. Convert JSON with Swagger-specification to HTML ****"
nix-shell -p nodejs-7_x --run "npm install bootprint bootprint-openapi html-inline"
# We need add it in PATH to run it.
PATH=$PATH:$(pwd)/node_modules/.bin
nix-shell -p nodejs-7_x --run "bootprint openapi ${SWAGGER_WALLET_API_JSON_SPEC} ${WALLET_API_PRODUCED_ROOT}"
nix-shell -p nodejs-7_x --run "html-inline ${WALLET_API_PRODUCED_ROOT}/${WALLET_API_HTML} > ${WALLET_API_HTML}"

echo "**** 3. Cloning cardanodocs.com repository ****"
# Variable ${GITHUB_CARDANO_DOCS_ACCESS} already stored in Travis CI settings for 'cardano-sl' repository.
# This token gives us an ability to push into docs repository.

rm -rf "${CARDANO_DOCS_REPO}"
# We need `master` only, because Jekyll builds docs from `master` branch.
git clone --quiet --branch=master \
    https://"${GITHUB_CARDANO_DOCS_ACCESS_2}"@github.com/input-output-hk/cardanodocs.com \
    "${CARDANO_DOCS_REPO}"

echo "**** 4. Copy (probably new) version of docs ****"
mv "${WALLET_API_HTML}" "${CARDANO_DOCS_REPO}"/"${WALLET_API_ROOT}"/

echo "**** 5. Push all changes ****"
cd "${CARDANO_DOCS_REPO}"
git add .
if [ -n "$(git status --porcelain)" ]; then 
    echo "     There are changes in Wallet Web API docs, push it";
    git commit -a -m "Automatic Wallet Web API docs rebuilding."
    git push --force origin master
    # After we push new docs in `master`,
    # Jekyll will automatically rebuild it on cardanodocs.com website.
else
    echo "     No changes in Wallet Web API docs, skip.";
fi
