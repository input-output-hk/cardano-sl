update_NIX_PATH() {
  local readlink
  local scriptDir
  readlink=$(nix-instantiate --eval -E "/. + (import <nix/config.nix>).coreutils")/readlink
  scriptDir=$(dirname -- "$("$readlink" -f -- "${BASH_SOURCE[0]}")")
  NIX_PATH="nixpkgs=$(nix-build "${scriptDir}/../fetch-nixpkgs.nix" --no-out-link)"
  export NIX_PATH
}
update_NIX_PATH
