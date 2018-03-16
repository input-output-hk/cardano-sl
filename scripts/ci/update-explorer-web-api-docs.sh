#!/usr/bin/env nix-shell
#! nix-shell -j 4 -i bash -p stack git
#! nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/cb90e6a0361554d01b7a576af6c6fae4c28d7513.tar.gz
export NIX_PATH=nixpkgs=https://github.com/NixOS/nixpkgs/archive/cb90e6a0361554d01b7a576af6c6fae4c28d7513.tar.gz


set -euo pipefail

echo "Cardano SL Explorer Web API updating"

readonly CARDANO_DOCS_REPO="${HOME}"/cardanodocs
readonly SWAGGER_EXPLORER_API_JSON_SPEC=explorer-web-api-swagger.json
readonly EXPLORER_API_PRODUCED_ROOT=explorer-web-api
readonly EXPLORER_API_HTML=index.html
readonly EXPLORER_API_ROOT=technical/explorer/api

echo "**** 1. Get Swagger-specification for explorer web API ****"
stack exec --nix -- cardano-explorer-swagger
# Done, 'SWAGGER_EXPLORER_API_JSON_SPEC' file is already here.

echo "**** 2. Convert JSON with Swagger-specification to HTML ****"
nix-shell -p nodejs-7_x --run "npm install bootprint bootprint-openapi html-inline"
# We need add it in PATH to run it.
PATH=$PATH:$(pwd)/node_modules/.bin
nix-shell -p nodejs-7_x --run "bootprint openapi ${SWAGGER_EXPLORER_API_JSON_SPEC} ${EXPLORER_API_PRODUCED_ROOT}"
nix-shell -p nodejs-7_x --run "html-inline ${EXPLORER_API_PRODUCED_ROOT}/${EXPLORER_API_HTML} > ${EXPLORER_API_HTML}"

echo "**** 3. Cloning cardanodocs.com repository ****"
# Variable ${GITHUB_CARDANO_DOCS_ACCESS_2} must be set by the CI system.
# This token gives us an ability to push into docs repository.

rm -rf "${CARDANO_DOCS_REPO}"
# We need `master` only, because Jekyll builds docs from `master` branch.
git clone --quiet --branch=master \
    https://"${GITHUB_CARDANO_DOCS_ACCESS_2}"@github.com/input-output-hk/cardanodocs.com \
    "${CARDANO_DOCS_REPO}"

echo "**** 4. Copy (probably new) version of docs ****"
mv "${EXPLORER_API_HTML}" "${CARDANO_DOCS_REPO}"/"${EXPLORER_API_ROOT}"/

echo "**** 5. Push all changes ****"
cd "${CARDANO_DOCS_REPO}"
git add .
if [ -n "$(git status --porcelain)" ]; then
    echo "     There are changes in Explorer Web API docs, push it";
    git commit -a -m "Automatic Explorer Web API docs rebuilding."
    git push --force origin master
    # After we push new docs in `master`,
    # Jekyll will automatically rebuild it on cardanodocs.com website.
else
    echo "     No changes in Explorer Web API docs, skip.";
fi
