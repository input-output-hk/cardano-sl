let
  lib = (import ./lib.nix).pkgs.lib;
  commitIdFromGitRepo = import ./nix/commit-id.nix { inherit lib; };
in { customConfig ? {}
, target ? builtins.currentSystem
, gitrev ? commitIdFromGitRepo ./.git
, genesisArgs ? {}
}:
#
#
# Generated targets include anything from stack.yaml (via nix-tools:stack-to-nix and the nix/regenerate.sh script)
# or cabal.project (via nix-tools:plan-to-nix), including all
# version overrides specified there.
#
# Nix-tools stack-to-nix will generate the `nix/.stack-pkgs.nix`
# file which is imported from the `nix/pkgs.nix` where further
# customizations outside of the ones in stack.yaml/cabal.project
# can be specified as needed for nix/ci.
#
# Please run `nix/regenerate.sh` after modifying stack.yaml
# or relevant part of cabal configuration files.
# When switching to recent stackage or hackage package version,
# you might also need to update the iohk-nix common lib. You
# can do so by running the `nix/update-iohk-nix.sh` script.
#
# More information about iohk-nix and nix-tools is available at:
# https://github.com/input-output-hk/iohk-nix/blob/master/docs/nix-toolification.org#for-a-stackage-project
#

let
  system = if target != "x86_64-windows" then target else builtins.currentSystem;
  crossSystem = if target == "x86_64-windows" then lib.systems.examples.mingwW64 else null;
  # commonLib provides iohk-nix tooling and extra libraries specific to cardano-sl.
  commonLib = import ./lib.nix;
  pkgs = import commonLib.nixpkgs { inherit system crossSystem; };
  src = commonLib.cleanSourceHaskell ./.;

  # nixTools contains all the haskell binaries and libraries built by haskell.nix
  nixTools = import ./nix/nix-tools.nix { inherit system crossSystem; };
  cardanoConfig = pkgs.callPackage ./nix/cardano-sl-config.nix {};
  # scripts contains connectScripts, dockerImages and demoCluster
  scripts = import ./nix/scripts.nix {
    inherit commonLib nixTools customConfig cardanoConfig;
  };
  mkGenesis = pkgs.callPackage ./scripts/prepare-genesis (genesisArgs // {
    inherit (nixTools.nix-tools.libs) cardano-sl;
    inherit (nixTools.nix-tools.exes) cardano-sl-tools;
  });
  # Tests contains code quality tests like shellcheck, yaml validation, and haskell style requirements to pass CI
  tests = import ./nix/tests.nix {
    inherit commonLib src nixTools;
  };
  # daedalus bridge contains the binaries and config files daedalus requires
  daedalus-bridge = pkgs.callPackage ./nix/daedalus-bridge.nix {
    inherit nixTools cardanoConfig gitrev;
    version = nixTools.nix-tools._raw.cardano-sl.identifier.version;
  };
  explorerFrontend = pkgs.callPackage ./explorer/frontend {
    inherit gitrev;
    cardano-sl-explorer = nixTools.nix-tools.exes.cardano-sl-explorer;
  };
  explorerPythonAPI = pkgs.callPackage ./explorer/python-api {
  };
  faucetFrontend = pkgs.callPackage ./faucet/frontend { };
  # Currently the only acceptance tests here are to sync the wallet
  # against mainnet and testnet.
  acceptanceTests = let
    acceptanceTest = args: pkgs.callPackage ./nix/acceptance {
      inherit (scripts) connect;
      inherit (args) environment resume;
    };
    mkTest = environment: {
      full  = acceptanceTest { environment = environment.name; resume = false; };
      quick = acceptanceTest { environment = environment.name; resume = true; };
    };
  in commonLib.forEnvironments mkTest;
in {
  inherit pkgs acceptanceTests daedalus-bridge tests mkGenesis
          cardanoConfig faucetFrontend explorerFrontend explorerPythonAPI;
  inherit (nixTools) nix-tools;
} // scripts
