let
  fixedNixpkgs = (import ./lib.nix).fetchNixPkgs;
in
  { supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]
  , scrubJobs ? true
}:
with import (fixedNixpkgs + "/pkgs/top-level/release-lib.nix") { inherit supportedSystems scrubJobs; packageSet = import ./.; };
let
  lib = import ./lib.nix;
  pkgs = import lib.fetchNixPkgs { config={}; };
  cardano = import ./. { inherit pkgs; };
  jobs = mapTestOn (packagePlatforms cardano);
in {
  inherit (jobs) cardano-sl cardano-sl-static;
}
