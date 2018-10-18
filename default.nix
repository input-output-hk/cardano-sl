########################################################################
# default.nix -- The top-level nix build file for cardano-sl.
#
# This file defines an attribute set of cardano-sl packages.
#
# It contains:
#
#   - pkgs -- the nixpkgs set that the build is based on.
#   - haskellPackages.* -- the package set based on stackage
#   - haskellPackages.ghc -- the compiler
#   - cardanoPackages -- just cardano packages
#
#   - tests -- integration tests and linters suitable for running in a
#              sandboxed build environment
#
#   - acceptanceTests -- tests which need network access to connect to
#                        the actual relay nodes.
#
#   - dockerImages -- for exchanges and developers who like docker
#                     Build these and `docker load -i` the resulting
#                     file.
#      - mainnet
#      - staging
#      - testnet
#
#   - connectScripts -- builds a script which starts a wallet. Run the
#                       resulting script.
#      - mainnet.wallet   -- connect a wallet to mainnet
#      - mainnet.explorer -- explorer node connected to testnet
#      - staging.*      -- connect scripts for staging
#      - testnet.*      -- connect scripts for testnet
#
# Other files:
#   - shell.nix   - dev environment, used by nix-shell / nix run.
#   - release.nix - the Hydra jobset.
#   - lib.nix     - the localLib common functions.
#   - nix/*       - other nix code modules used by this file.
#
# See also:
#   - docs/how-to/build-cardano-sl-and-daedalus-from-source-code.md
#   - docs/nix.md
#
########################################################################

let
  localLib = import ./lib.nix;
in
{ system ? builtins.currentSystem
, config ? {}  # The nixpkgs configuration file

# Use a pinned version nixpkgs.
, pkgs ? localLib.importPkgs { inherit system config; }

# SHA1 hash which will be embedded in binaries
, gitrev ? localLib.commitIdFromGitRepo ./.git

# This is set by CI
, buildId ? null

# Disable running of tests for all cardano-sl packages.
, forceDontCheck ? false

# Enable profiling for all haskell packages.
# Profiling slows down performance by 50% so we don't enable it by default.
, enableProfiling ? false

# Keeps the debug information for all haskell packages.
, enableDebugging ? false

# Build (but don't run) benchmarks for all cardano-sl packages.
, enableBenchmarks ? true

# Overrides all nix derivations to add build timing information in
# their build output.
, enablePhaseMetrics ? true

# Disables optimization in the build for all cardano-sl packages.
, fasterBuild ? false

# Whether local options in ./custom-wallet-config.nix should apply to
# the wallet connect script build.
, allowCustomConfig ? true

# Makes the demo wallet/cluster/connect scripts use "stack exec"
# instead of running the nix-built executables.
, useStackBinaries ? false
}:

with pkgs.lib;

let
  src = localLib.cleanSourceTree ./.;

  packages = self: ({
    inherit pkgs;

    # This is the stackage LTS plus overrides, plus the cardano-sl
    # packages.
    haskellPackages = self.callPackage ./nix/haskell-packages.nix {
      inherit forceDontCheck enableProfiling enablePhaseMetrics
        enableBenchmarks fasterBuild enableDebugging;
    };

    # fixme: I would like to have these attributes at the top-level,
    # but am getting problems with infinite recursion. Help me!
    cardanoPackages = localLib.getCardanoPackages self.justStaticExecutablesGitRev self.haskellPackages;

    # fixme: this is just for CI so should probably only be in release.nix
    all-cardano-sl = pkgs.buildEnv {
      name = "all-cardano-sl";
      paths = attrValues self.cardanoPackages;
      ignoreCollisions = true;
    };


    ####################################################################
    # Frontends

    # The explorer frontend, built with Purescript.
    cardano-sl-explorer-frontend = self.callPackage ./explorer/frontend {
      cardano-sl-explorer = self.cardanoPackages.cardano-sl-explorer-static;
    };

    # A demo/development frontend for the faucet API. Override this
    # derivation to customize URLs, etc.
    cardano-sl-faucet-frontend = self.callPackage ./faucet/frontend { };
    # Backwards compat for iohk-ops.
    makeFaucetFrontend = self.cardano-sl-faucet-frontend;

    ####################################################################
    # Report Server

    cardano-report-server-static = self.justStaticExecutablesGitRev self.cardano-report-server;


    ####################################################################
    # Daedalus wallet

    # Packages all the configuration required for running a node.
    cardano-sl-config = self.callPackage ./nix/cardano-sl-config.nix { };

    # Provides the edge node (wallet), tools, and configuration
    # required for Daedalus.
    daedalus-bridge = self.callPackage ./nix/daedalus-bridge.nix {
      cardano-sl-node = self.cardanoPackages.cardano-sl-node-static;
      cardano-sl-tools = self.cardanoPackages.cardano-sl-tools-static;
      cardano-sl-wallet-new = self.cardanoPackages.cardano-sl-wallet-new-static;
    };


    ####################################################################
    # Docker images

    dockerImages = let
      build = args: self.callPackage ./nix/docker.nix ({
        inherit (self.cardanoPackages) cardano-sl-node-static;
      } // args);
      makeDockerImage = { environment, ...}:
        build { inherit environment; } // {
          wallet   = build { inherit environment; type = "wallet"; };
          explorer = build { inherit environment; type = "explorer"; };
          node     = build { inherit environment; type = "node"; };
        };
    in localLib.forEnvironments makeDockerImage;

    ####################################################################
    # Tests

    tests = {
      shellcheck = self.callPackage ./scripts/test/shellcheck.nix { inherit src; };
      hlint = self.callPackage ./scripts/test/hlint.nix { inherit src; };
      stylishHaskell = self.callPackage ./scripts/test/stylish.nix { inherit (self.haskellPackages) stylish-haskell; inherit src; };
      walletIntegration = self.callPackage ./scripts/test/wallet/integration/build-test.nix { };
      swaggerSchemaValidation = self.callPackage ./scripts/test/wallet/swaggerSchemaValidation.nix {
        inherit (self.cardanoPackages) cardano-sl-wallet-new;
      };
      yamlValidation = self.callPackage ./scripts/test/yamlValidation.nix {
        inherit (self) haskellPackages; inherit (localLib) runHaskell;
      };
    };

    walletIntegrationTests = self.callPackage ./scripts/test/wallet/integration {
      inherit (self.cardanoPackages)
        cardano-sl-tools
        cardano-sl-wallet-new;
      inherit useStackBinaries;
    };

    # Currently the only acceptance tests here are to sync the wallet
    # against mainnet and testnet.
    acceptanceTests = let
      acceptanceTest = args: self.callPackage ./scripts/test/acceptance ({
        inherit (self.cardanoPackages)
          cardano-sl-tools
          cardano-sl-wallet-new;
      } // args);
      mkTest = { environment, ...}: {
        full  = acceptanceTest { inherit environment; resume = false; };
        quick = acceptanceTest { inherit environment; resume = true; };
      };
    in localLib.forEnvironments mkTest;

    ####################################################################
    ## Connect scripts and demo cluster

    # A function to connect a wallet to a network.
    # The args parameter is an attrset for parameters applied to
    # ./scripts/launch/connect-to-cluster/default.nix
    connect = let
      walletConfigFile = ./custom-wallet-config.nix;
      walletConfig = if allowCustomConfig
        then (if builtins.pathExists walletConfigFile then import walletConfigFile else {})
        else {};
      in
        args: self.callPackage ./scripts/launch/connect-to-cluster (args // {
          inherit (self.cardanoPackages)
            cardano-sl-wallet-new-static
            cardano-sl-explorer-static
            cardano-sl-tools-static;
          inherit useStackBinaries;
        } // walletConfig);

    # Connect scripts for each network
    connectScripts = localLib.forEnvironments ({ environment, ... }: {
      wallet = self.connect { inherit environment; };
      explorer = self.connect { inherit environment; executable = "explorer"; };
    });

    demoCluster = self.callPackage ./scripts/launch/demo-cluster {
      inherit useStackBinaries;
      inherit (self.cardanoPackages)
        cardano-sl
        cardano-sl-tools
        cardano-sl-wallet-new-static
        cardano-sl-node-static;
    };

    ####################################################################
    # Build tools

    # Utility which removes all but the statically linked executables
    # of a haskell package, and stamps them with the git revision.
    justStaticExecutablesGitRev = self.callPackage ./scripts/set-git-rev {
      inherit (self.haskellPackages) ghc;
      inherit gitrev;
    };

    # Tool for generating ./pkgs/default.nix
    stack2nix = self.callPackage ./nix/stack2nix.nix { };

    validateJson = self.callPackage ./tools/src/validate-json {};

    # Add a shell attribute so that it can be built and cached by Hydra.
    shell = import ./shell.nix { inherit system config pkgs; iohkPkgs = self; };


    ####################################################################
    # Version info

    inherit (self.haskellPackages.cardano-sl) version;
    inherit gitrev;
  }
   # fixme: Temporary fix for hydra evaluation
   // { inherit (self.cardanoPackages)
          cardano-sl
          cardano-sl-auxx
          cardano-sl-chain
          cardano-sl-cluster
          cardano-sl-core
          cardano-sl-crypto
          cardano-sl-db
          cardano-sl-explorer
          cardano-sl-explorer-static
          cardano-sl-generator
          cardano-sl-infra
          cardano-sl-networking
          cardano-sl-node-static
          cardano-sl-tools
          cardano-sl-tools-post-mortem
          cardano-sl-util
          cardano-sl-wallet
          cardano-sl-wallet-new
          cardano-sl-x509;
        inherit (self.haskellPackages)
          cardano-report-server; }

  );

in
  # The top-level package set
  pkgs.lib.makeScope pkgs.newScope packages
