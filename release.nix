let
  fixedNixpkgs = (import ./lib.nix).fetchNixPkgs;
in
  { supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]
  , scrubJobs ? false
  , dconfigs ? [ "testnet_staging_full" "testnet_staging_wallet" ]
  , cardano ? { outPath = ./.; rev = "abcdef"; }
  }:

let
  lib = import ./lib.nix;
  mergeAttrsMap = f: attrs: lib.foldl (x: y: x // (f y)) {} attrs;
  rlib = import (fixedNixpkgs + "/pkgs/top-level/release-lib.nix") { inherit supportedSystems scrubJobs; };
  withDconfig = dconfig: import (fixedNixpkgs + "/pkgs/top-level/release-lib.nix") { 
    inherit supportedSystems scrubJobs; 
    packageSet = import ./.;
    nixpkgsArgs = { 
      inherit dconfig; 
      gitrev = cardano.rev;
      config = { allowUnfree = false; inHydra = true; };
    };
  };
  platforms = {
    cardano-sl = supportedSystems;
    cardano-sl-static = supportedSystems;
    cardano-sl-tools = supportedSystems;
    cardano-sl-explorer-static = [ "x86_64-linux" ];
    cardano-report-server-static = [ "x86_64-linux" ];
  };
in (mergeAttrsMap (dconfig: { ${dconfig } = (withDconfig dconfig).mapTestOn platforms; }) dconfigs)
   // ((withDconfig null).mapTestOn { stack2nix = supportedSystems; })
   // (rlib.mapTestOn { purescript = supportedSystems; })
