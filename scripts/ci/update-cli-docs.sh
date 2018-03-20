#!/bin/bash
set -euo pipefail

echo "Cardano SL CLI docs updating"

readonly CARDANO_DOCS_REPO="${HOME}"/cardanodocs
readonly DOCUMENTER=cardano-cli-docs
readonly RAW_CHAPTER_NAME="${DOCUMENTER}".md
readonly PATH_TO_REAL_CHAPTER="${CARDANO_DOCS_REPO}"/_docs/technical/2017-01-15-cli-options.md
readonly PATH_TO_DIR_WITH_EXECUTABLES=$(stack path --local-install-root)/bin

echo "**** 1. Get raw Markdown chapter ****"
stack exec --nix -- "${DOCUMENTER}" --bin-dir "${PATH_TO_DIR_WITH_EXECUTABLES}"
# Done, 'RAW_CHAPTER_NAME' file is already here.

echo "**** 2. Cloning cardanodocs.com repository ****"
# Variable ${GITHUB_CARDANO_DOCS_ACCESS_2} must be set by the CI system.
# This token gives us an ability to push into docs repository.

rm -rf "${CARDANO_DOCS_REPO}"
# We need `master` only, because Jekyll builds docs from `master` branch.
git clone --quiet --branch=master \
    https://"${GITHUB_CARDANO_DOCS_ACCESS_2}"@github.com/input-output-hk/cardanodocs.com \
    "${CARDANO_DOCS_REPO}"

echo "**** 3. Copy (probably new) version of this chapter ****"
mv "${RAW_CHAPTER_NAME}" "${PATH_TO_REAL_CHAPTER}"

echo "**** 4. Push all changes ****"
cd "${CARDANO_DOCS_REPO}"
git add .
if [ -n "$(git status --porcelain)" ]; then 
    echo "     There are changes in CLI-docs chapter, push it";
    git commit -a -m "Automatic CLI-docs chapter rebuilding."
    git push --force origin master
    # After we push new docs in `master`,
    # Jekyll will automatically rebuild it on cardanodocs.com website.
else
    echo "     No changes in CLI-docs chapter, skip.";
fi
