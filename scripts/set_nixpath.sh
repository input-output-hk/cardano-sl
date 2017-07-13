update_NIX_PATH() {
  # `readlink -f` doesn't work on darwin, but the coreutils in nix fixes it
  case $OSTYPE in darwin*)
    nix-build -o /tmp/iohk-utils '<nixpkgs>' -A coreutils
    export PATH=/tmp/iohk-utils/bin:$PATH
  esac
  local scriptDir="$(dirname -- "$(readlink -f -- "${BASH_SOURCE[0]}")")"
  export NIX_PATH=nixpkgs=https://github.com/NixOS/nixpkgs/archive/$(nix-shell -p jq --run "jq .rev < \"${scriptDir}/../nixpkgs-src.json\" -r").tar.gz
  # allows the nix expressions to detect that the path is already fixed
  export NIX_PATH_LOCKED=1
}
update_NIX_PATH
