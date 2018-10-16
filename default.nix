let
  localLib = import ./lib.nix;
in
{ system ? builtins.currentSystem
, config ? {}
, pkgs ? (import (localLib.fetchNixPkgs) { inherit system config; overlays = [ (import ./nix/overlays/jemalloc.nix) ]; })
, gitrev ? localLib.commitIdFromGitRepo ./.git
, buildId ? null
, forceDontCheck ? false
# profiling slows down performance by 50% so we don't enable it by default
, enableProfiling ? false
, enableDebugging ? false
, enableBenchmarks ? true
, enablePhaseMetrics ? true
, allowCustomConfig ? true
, useStackBinaries ? false
, fasterBuild ? false
}:

with pkgs.lib;

let
  # the GHC we are using
  # at some point use: pkgs.haskell.compiler.ghc843;
  ghc = overrideDerivation pkgs.haskell.compiler.ghc822 (drv: {
    patches = drv.patches ++ [ ./ghc-8.0.2-darwin-rec-link.patch ];
  });

  # Overlay logic for *haskell* packages.
  requiredOverlay    = import ./nix/overlays/required.nix     { inherit pkgs localLib enableProfiling gitrev;
                                                                inherit (cardanoPkgs) ghc; };
  benchmarkOverlay   = import ./nix/overlays/benchmark.nix    { inherit pkgs localLib; };
  debugOverlay       = import ./nix/overlays/debug.nix        { inherit pkgs; };
  fasterBuildOverlay = import ./nix/overlays/faster-build.nix { inherit pkgs localLib; };
  dontCheckOverlay   = import ./nix/overlays/dont-check.nix   { inherit pkgs; };
  metricOverlay      = import ./nix/overlays/metric.nix       { inherit pkgs; };

  # This will yield a set of haskell packages, based on the given compiler.
  cardanoPkgsBase = ((import ./pkgs { inherit pkgs; }).override {
    inherit ghc;
  });

  activeOverlays = [ requiredOverlay ]
      ++ optional enablePhaseMetrics metricOverlay
      ++ optional enableBenchmarks benchmarkOverlay
      ++ optional enableDebugging debugOverlay
      ++ optional forceDontCheck dontCheckOverlay
      ++ optional fasterBuild fasterBuildOverlay;

  cardanoPkgs = builtins.foldl' (pkgs: overlay: pkgs.extend overlay) cardanoPkgsBase activeOverlays;
  connect = let
      walletConfigFile = ./custom-wallet-config.nix;
      walletConfig = if allowCustomConfig then (if builtins.pathExists walletConfigFile then import walletConfigFile else {}) else {};
    in
      args: pkgs.callPackage ./scripts/launch/connect-to-cluster (args // { inherit gitrev useStackBinaries; } // walletConfig );
  other = rec {
    inherit pkgs;
    testlist = innerClosePropagation [] [ cardanoPkgs.cardano-sl ];
    walletIntegrationTests = pkgs.callPackage ./scripts/test/wallet/integration { inherit gitrev forceDontCheck useStackBinaries; };
    validateJson = pkgs.callPackage ./tools/src/validate-json {};
    demoCluster = pkgs.callPackage ./scripts/launch/demo-cluster {
      inherit gitrev useStackBinaries;
      iohkPkgs = cardanoPkgs;
    };
    demoClusterLaunchGenesis = pkgs.callPackage ./scripts/launch/demo-cluster {
      inherit gitrev useStackBinaries;
      iohkPkgs = cardanoPkgs;
      launchGenesis = true;
      configurationKey = "testnet_full";
      runWallet = false;
    };
    tests = let
      src = localLib.cleanSourceTree ./.;
    in {
      shellcheck = pkgs.callPackage ./scripts/test/shellcheck.nix { inherit src; };
      hlint = pkgs.callPackage ./scripts/test/hlint.nix { inherit src; };
      stylishHaskell = pkgs.callPackage ./scripts/test/stylish.nix { inherit (cardanoPkgs) stylish-haskell; inherit src localLib; };
      walletIntegration = pkgs.callPackage ./scripts/test/wallet/integration/build-test.nix { inherit walletIntegrationTests; };
      swaggerSchemaValidation = pkgs.callPackage ./scripts/test/wallet/swaggerSchemaValidation.nix { inherit gitrev; };
      yamlValidation = pkgs.callPackage ./scripts/test/yamlValidation.nix { inherit cardanoPkgs; inherit (localLib) runHaskell; };
    };
    cardano-sl-explorer-frontend = (import ./explorer/frontend {
      inherit system config gitrev pkgs;
      cardano-sl-explorer = cardanoPkgs.cardano-sl-explorer-static;
    });
    makeFaucetFrontend = pkgs.callPackage ./faucet/frontend;

    all-cardano-sl = pkgs.buildEnv {
      name = "all-cardano-sl";
      paths = attrValues (filterAttrs (name: drv: localLib.isCardanoSL name) cardanoPkgs);
      ignoreCollisions = true;
    };
    mkDocker = { environment, name ? "wallet", connectArgs ? {} }: import ./docker.nix { inherit environment name connect gitrev pkgs connectArgs; };
    stack2nix = import (pkgs.fetchFromGitHub {
      owner = "avieth";
      repo = "stack2nix";
      rev = "c51db2d31892f7c4e7ff6acebe4504f788c56dca";
      sha256 = "10jcj33sxpq18gxf3zcck5i09b2y4jm6qjggqdlwd9ss86wg3ksb";
    }) { inherit pkgs; };
    inherit (pkgs) purescript;
    connectScripts = {
      mainnet = {
        wallet = connect { };
        explorer = connect { executable = "explorer"; };
      };
      staging = {
        wallet = connect { environment = "mainnet-staging"; };
        explorer = connect { executable = "explorer"; environment = "mainnet-staging"; };
      };
      testnet = {
        wallet = connect { environment = "testnet"; };
        explorer = connect { executable = "explorer"; environment = "testnet"; };
      };
      demoWallet = connect { environment = "demo"; };
    };
    dockerImages = {
      mainnet.wallet = mkDocker { environment = "mainnet"; };
      staging.wallet = mkDocker { environment = "mainnet-staging"; };
      testnet.wallet = mkDocker { environment = "testnet"; };
      mainnet.explorer = mkDocker { environment = "mainnet"; name = "explorer"; connectArgs = { executable = "explorer"; }; };
      staging.explorer = mkDocker { environment = "mainnet-staging"; name = "explorer"; connectArgs = { executable = "explorer"; }; };
      testnet.explorer = mkDocker { environment = "testnet"; name = "explorer"; connectArgs = { executable = "explorer"; }; };
    };
    acceptanceTests = let
      acceptanceTest = args: pkgs.callPackage ./scripts/test/acceptance (args // { inherit gitrev forceDontCheck; });
      mkTest = { environment, ...}: {
        full  = acceptanceTest { inherit environment; resume = false; };
        quick = acceptanceTest { inherit environment; resume = true; };
      };
    in localLib.forEnvironments mkTest;

    shell = import ./shell.nix { inherit system config pkgs; iohkPkgs = cardanoPkgs; };

    cardano-sl-config = pkgs.runCommand "cardano-sl-config" {} ''
      mkdir -p $out/lib
      cp -R ${./log-configs} $out/log-configs
      cp ${./lib}/configuration.yaml $out/lib
      cp ${./lib}/*genesis*.json $out/lib
    '';
    daedalus-bridge = let
      inherit (cardanoPkgs.cardano-sl-node) version;
    in pkgs.runCommand "cardano-daedalus-bridge-${version}" {
      inherit version gitrev buildId;
    } ''
      # Generate daedalus-bridge
      mkdir -p $out/bin
      cd $out
      ${optionalString (buildId != null) "echo ${buildId} > build-id"}
      echo ${gitrev} > commit-id
      echo ${version} > version

      cp --no-preserve=mode -R ${cardano-sl-config}/lib config
      cp ${cardano-sl-config}/log-configs/daedalus.yaml $out/config/log-config-prod.yaml
      cp ${cardanoPkgs.cardano-sl-tools}/bin/cardano-launcher bin
      cp ${cardanoPkgs.cardano-sl-tools}/bin/cardano-x509-certificates bin
      cp ${cardanoPkgs.cardano-sl-wallet-new}/bin/cardano-node bin

      # test that binaries exit with 0
      ./bin/cardano-node --help > /dev/null
      HOME=$TMP ./bin/cardano-launcher --help > /dev/null
    '';
  };
in cardanoPkgs // other
