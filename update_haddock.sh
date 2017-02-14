#!/bin/bash
set -euo pipefail

echo "Haddock updating"

echo "**** 1. Rebuild documentation ****"
stack --nix --no-terminal haddock --no-haddock-deps

readonly PROJECT_NAME=$(cat ./*.cabal | grep "name:" | awk '{ print $2 }')
readonly PROJECT_VERSION=$(cat ./*.cabal | grep "version:" | grep -v "cabal" | awk '{ print $2 }')
readonly PROJECT_FULL_NAME="${PROJECT_NAME}"-"${PROJECT_VERSION}"

readonly DOC_ROOT=$(stack --nix --no-terminal path --local-doc-root)
readonly PROJECT_DOC_DIR="${DOC_ROOT}"/"${PROJECT_FULL_NAME}"

readonly CARDANO_DOCS_REPO="${HOME}"/cardano-docs
readonly LATEST_ROOT=haddock/latest
readonly RELEASE_ROOT=haddock/release

readonly CURRENT_BRANCH="${TRAVIS_BRANCH}"

echo "**** 2. Cloning cardano-docs.iohk.io repository ****"
# Variable ${GITHUB_CARDANO_DOCS_ACCESS} already stored in Travis CI settings for 'cardano-sl' repository.
# This token gives us an ability to push into docs repository.

rm -rf "${CARDANO_DOCS_REPO}"
cd "${HOME}"
# We need `master` only, because Jekyll builds docs from `master` branch.
git clone --quiet --branch=master \
    https://"${GITHUB_CARDANO_DOCS_ACCESS}"@github.com/input-output-hk/cardano-docs.iohk.io \
    "${CARDANO_DOCS_REPO}"
cd "${CARDANO_DOCS_REPO}"

echo "**** 3. Update latest Haddock-version ****"
mkdir -p "${LATEST_ROOT}"
cd "${LATEST_ROOT}"
rm -rf ./*
rsync -r "${PROJECT_DOC_DIR}"/ .

echo "**** 4. Check release version ****"
if [ "${CURRENT_BRANCH}" = "${PROJECT_FULL_NAME}" ]; then
    echo "     it's a release branch ${CURRENT_BRANCH}, preparing release documentation..."
    # 1. Place just generated docs in corresponding haddock-release subdir,
    cd "${CARDANO_DOCS_REPO}"
    mkdir -p "${RELEASE_ROOT}"
    rm -rf "${PROJECT_VERSION}"
    mkdir "${PROJECT_VERSION}"
    cd "${PROJECT_VERSION}"
    rsync -r "${PROJECT_DOC_DIR}"/ .

    # 2. Add href in Haddock-page (cardano-docs.iohk.io/for-contributors/haddock).
    # If this href already exists, skip this step.
    readonly HREF_TO_RELEASE_DOCS=/"${RELEASE_ROOT}"/"${PROJECT_VERSION}"/index.html
    readonly PATH_TO_HADDOCK_PAGE=_docs/for-contributors/haddock.md
    readonly LINK_TO_RELEASE=$(cat "${PATH_TO_HADDOCK_PAGE}" | grep "${HREF_TO_RELEASE_DOCS}")

    cd "${CARDANO_DOCS_REPO}"
    if [ -n "${LINK_TO_RELEASE}" ]; then
        echo "Link to this release docs already here, skip."
    else
        echo "There's no link to this release docs, adding it..."
        readonly MARKDOWN_HREF_TO_RELEASE_DOCS="* [${PROJECT_VERSION}](${HREF_TO_RELEASE_DOCS})"
        echo "${MARKDOWN_HREF_TO_RELEASE_DOCS}" >> "${PATH_TO_HADDOCK_PAGE}"
    fi
fi

echo "**** 5. Push all changes ****"
cd "${CARDANO_DOCS_REPO}"
git add .
if [ -n "$(git status --porcelain)" ]; then 
    echo "     There are changes in Haddock-docs, push it";
    git commit -a -m "Automatic Haddock docs rebuilding."
    git push origin master
    # After we push new docs in `master`,
    # Jekyll will automatically rebuild it on cardano-docs.iohk.io website.
else
    echo "     No changes in Haddock-docs, skip.";
fi
