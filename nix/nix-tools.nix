{ ... }@args:

let
  commonLib = import ./iohk-common.nix;

in commonLib.nix-tools.default-nix ./pkgs.nix args
