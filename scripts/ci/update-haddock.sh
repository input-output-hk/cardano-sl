#!/bin/bash
set -euo pipefail

echo "Haddock updating"

readonly PROJECT_NAME=$(cat ./*.cabal | grep "name:" | awk '{ print $2 }')
readonly PROJECT_VERSION=$(cat ./*.cabal | grep "version:" | grep -v "cabal" | awk '{ print $2 }')
readonly PROJECT_FULL_NAME="${PROJECT_NAME}"-"${PROJECT_VERSION}"

readonly DOC_ROOT=$(stack --nix --no-terminal path --local-doc-root)
readonly PROJECT_DOC_DIR="${DOC_ROOT}"/"${PROJECT_FULL_NAME}"

readonly CARDANO_DOCS_REPO="${HOME}"/cardanodocs
readonly LATEST_ROOT=haddock/latest
readonly RELEASE_ROOT=haddock/release

readonly CURRENT_BRANCH="${BUILDKITE_BRANCH}"

echo "**** 2. Change external Haskell-related links to the Hackage-based ones ****"
sed -i 's/href="\.\.\/\([^/]*\)\//href="http:\/\/hackage.haskell.org\/package\/\1\/docs\//g' "${PROJECT_DOC_DIR}"/*.html

echo "**** 3. Cloning cardanodocs.com repository ****"
# Variable ${GITHUB_CARDANO_DOCS_ACCESS} must be set by the CI system.
# This token gives us an ability to push into docs repository.

rm -rf "${CARDANO_DOCS_REPO}"
cd "${HOME}"
# We need `master` only, because Jekyll builds docs from `master` branch.
git clone --quiet --branch=master \
    https://"${GITHUB_CARDANO_DOCS_ACCESS_2}"@github.com/input-output-hk/cardanodocs.com \
    "${CARDANO_DOCS_REPO}"
cd "${CARDANO_DOCS_REPO}"

echo "**** 4. Update latest Haddock-version ****"
mkdir -p "${LATEST_ROOT}"
cd "${LATEST_ROOT}"
rm -rf ./*
rsync -r "${PROJECT_DOC_DIR}"/ .

echo "**** 5. Check release version ****"
if [ "${CURRENT_BRANCH}" = "${PROJECT_FULL_NAME}" ]; then
    echo "     it's a release branch ${CURRENT_BRANCH}, preparing release documentation..."
    # 1. Place just generated docs in corresponding haddock-release subdir,
    cd "${CARDANO_DOCS_REPO}"
    mkdir -p "${RELEASE_ROOT}"
    rm -rf "${PROJECT_VERSION}"
    mkdir "${PROJECT_VERSION}"
    cd "${PROJECT_VERSION}"
    rsync -r "${PROJECT_DOC_DIR}"/ .

    # 2. Add href in Haddock-page (cardanodocs.com/for-contributors/haddock).
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

echo "**** 6. Push all changes ****"
cd "${CARDANO_DOCS_REPO}"
git add .
if [ -n "$(git status --porcelain)" ]; then 
    echo "     There are changes in Haddock-docs, push it";
    git commit -a -m "Automatic Haddock docs rebuilding."
    # Force-push to prevent race condition among few master-based CI-build. 
    git push -f origin master
    # After we push new docs in `master`,
    # Jekyll will automatically rebuild it on cardanodocs.com website.
else
    echo "     No changes in Haddock-docs, skip.";
fi
