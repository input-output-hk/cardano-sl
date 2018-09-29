let
  nixpkgs_path = import ./snack/nix/nixpkgs;
in
  with import nixpkgs_path {};
  with lib;
  let
    snack-src = ./snack;
    snack = pkgs.callPackage (snack-src + "/snack-lib") {};
    # todo, use overlays to fix the definitions up
    definitions = import ./snack.nix definitions;
  in builtins.mapAttrs (key: value: snack.mkPackage value) definitions
