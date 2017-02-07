set -xe
git submodule update --init --remote --recursive
pushd cardano-sl-explorer
stack --nix build
stack --nix exec -- cardano-explorer-hs2purs --bridge-path ../src/Generated/
popd
nix-shell --run ./scripts/generate-backend-lenses.sh
nix-shell --run ./scripts/generate-frontend-lenses.sh
nix-shell --run "npm install && npm run build:prod"
echo "Done. Generated ./dist/"
