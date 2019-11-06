# Provides a function for fetching a GitHub repo from a JSON spec.
{ name, specJSON }:

let
  spec = builtins.fromJSON (builtins.readFile specJSON);
in
  builtins.fetchTarball {
    inherit name;
    url = "${spec.url}/archive/${spec.rev}.tar.gz";
    inherit (spec) sha256;
  }
