let
  fixedNixpkgs = (import ./lib.nix).fetchNixPkgs;
in
  { supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]
  , scrubJobs ? true
  , dconfigs ? [ "testnet_staging" ]
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
      inherit (jobs) cardano-sl cardano-sl-static cardano-sl-tools cardano-sl-explorer-static stack2nix;
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
  cardano-jobs = (listToAttrs (map mkDconfigs dconfigs));
  tests = import ./tests { inherit pkgs; supportedSystems = [ "x86_64-linux" ]; };
  cardano = import ./. { inherit pkgs; };
in {
  inherit (cardano) make-genesis;
  inherit tests;
} // cardano-jobs
