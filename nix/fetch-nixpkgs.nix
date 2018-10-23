let
  spec = builtins.fromJSON (builtins.readFile ../nixpkgs-src.json);
in builtins.fetchTarball {
  url = "${spec.url}/archive/${spec.rev}.tar.gz";
  inherit (spec) sha256;
}
