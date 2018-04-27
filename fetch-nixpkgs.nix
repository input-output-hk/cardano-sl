let
  spec = builtins.fromJSON (builtins.readFile ./nixpkgs-src.json);
in
  import ./fetchNixpkgs.nix spec
