let
  fixedNixpkgs = (import ./lib.nix).fetchNixPkgs;
in
  { supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]
  , scrubJobs ? true
  , cardano ? { outPath = ./.; rev = "abcdef"; }
  , nixpkgsArgs ? {
      config = { allowUnfree = false; inHydra = true; }; 
      gitrev = cardano.rev;
    }
  }:

with (import (fixedNixpkgs + "/pkgs/top-level/release-lib.nix") {
  inherit supportedSystems scrubJobs nixpkgsArgs;
  packageSet = import ./.;
});

let
  platforms = {
    cardano-sl = supportedSystems;
    cardano-sl-static = supportedSystems;
    cardano-sl-tools = supportedSystems;
    cardano-sl-wallet = supportedSystems;
    cardano-sl-explorer-static = [ "x86_64-linux" ];
    cardano-report-server-static = [ "x86_64-linux" ];
    stack2nix = supportedSystems;
    purescript = supportedSystems;
    dockerImage = [ "x86_64-linux" ];
  };
  connect = import ./scripts/launch/connect-to-cluster/default.nix;
  connectScripts = {
    mainnetWallet = connect { inherit (cardano) rev; };
    mainnetExplorer = connect { inherit (cardano) rev; executable = "explorer"; };
    stagingWallet = connect { inherit (cardano) rev; environment = "mainnet-staging"; };
    stagingExplorer = connect { inherit (cardano) rev; executable = "explorer"; environment = "mainnet-staging"; };
  };
in { connect = connectScripts; }
   // mapTestOn platforms
