let
  # temporary hack until scripts/nix-shell.sh ceases to use -p
  pkgs_path = import ./fetchNixpkgs.nix (builtins.fromJSON (builtins.readFile ./nixpkgs-src.json));
  pkgs = import pkgs_path { config = {}; overlays = []; };
  wrapped = pkgs.runCommand "nixpkgs" {} ''
    ln -sv ${pkgs_path} $out
  '';
in if 0 <= builtins.compareVersions builtins.nixVersion "1.12" then wrapped else pkgs_path
