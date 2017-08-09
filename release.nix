let
  fixedNixpkgs = (import ./lib.nix).fetchNixPkgs;
in
  { supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]
  , scrubJobs ? true
  , dconfigs ? [ "testnet_staging" "travis" ]
}:
with import (fixedNixpkgs + "/pkgs/top-level/release-lib.nix") { inherit supportedSystems scrubJobs; packageSet = import ./.; };
with builtins;
let
  lib = import ./lib.nix;
  pkgs = import lib.fetchNixPkgs { config={}; };
  mkJob = dconfig: system: let
    jobs = import ./. { inherit system dconfig; };
  in {
    name = system;
    value = {
      inherit (jobs) cardano-sl cardano-sl-static cardano-sl-tools;
    };
  };
  mkJobs = dconfig: systems: listToAttrs (map (mkJob dconfig) systems);
  mkDconfigs = dconfig: let
    cardano = import ./. { inherit pkgs dconfig; };
    jobs = mkJobs dconfig supportedSystems;
  in {
    name = dconfig;
    value = jobs;
  };
  cardano = import ./. {};
in (listToAttrs (map mkDconfigs dconfigs)) // {
  inherit (cardano) stack2nix;
}
