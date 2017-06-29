update_NIX_PATH() {
  local scriptDir=$(dirname -- "$(readlink -f -- "${BASH_SOURCE[0]}")")
  export NIX_PATH=nixpkgs=https://github.com/NixOS/nixpkgs/archive/$(jq .rev < ${scriptDir}/../nixpkgs-src.json -r).tar.gz
  export NIX_PATH_LOCKED=1
}
update_NIX_PATH
