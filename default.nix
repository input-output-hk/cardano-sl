let
  localLib = import ./lib.nix;
  jemallocOverlay = self: super: {
    # jemalloc has a bug that caused cardano-sl-db to fail to link (via
    # rocksdb, which can use jemalloc).
    # https://github.com/jemalloc/jemalloc/issues/937
    # Using jemalloc 510 with the --disable-initial-exec-tls flag seems to
    # fix it.
    jemalloc = self.callPackage ./nix/jemalloc/jemalloc510.nix {};
  };
in
{ system ? builtins.currentSystem
, config ? {}
, gitrev ? localLib.commitIdFromGitRepo ./.git
, buildId ? null
, pkgs ? (import (localLib.fetchNixPkgs) { inherit system config; overlays = [ jemallocOverlay ]; })
# profiling slows down performance by 50% so we don't enable it by default
, forceDontCheck ? false
, enableProfiling ? false
, enableDebugging ? false
, enableBenchmarks ? true
, enablePhaseMetrics ? true
, allowCustomConfig ? true
, useStackBinaries ? false
}:

with pkgs.lib;
with pkgs.haskell.lib;

let
  justStaticExecutablesGitRev = import ./scripts/set-git-rev {
    inherit pkgs gitrev;
    inherit (cardanoPkgs) ghc;
  };
  addRealTimeTestLogs = drv: overrideCabal drv (attrs: {
    testTarget = "--show-details=streaming";
  });

  requiredOverlay = self: super: {
    srcroot = ./.;
    cardano-sl-core = overrideCabal super.cardano-sl-core (drv: {
      configureFlags = (drv.configureFlags or []) ++ [
        "-f-asserts"
      ];
    });
    cardano-sl = overrideCabal super.cardano-sl (drv: {
      # production full nodes shouldn't use wallet as it means different constants
      configureFlags = (drv.configureFlags or []) ++ [
        "-f-asserts"
      ];
      # waiting on load-command size fix in dyld
      doCheck = ! pkgs.stdenv.isDarwin;
      passthru = {
        inherit enableProfiling;
      };
    });
    cardano-sl-wallet-static = justStaticExecutablesGitRev super.cardano-sl-wallet;
    cardano-sl-client = addRealTimeTestLogs super.cardano-sl-client;
    cardano-sl-generator = addRealTimeTestLogs super.cardano-sl-generator;
    cardano-sl-auxx = justStaticExecutablesGitRev super.cardano-sl-auxx;
    cardano-sl-wallet-new-static = justStaticExecutablesGitRev self.cardano-sl-wallet-new;
    cardano-sl-node-static = justStaticExecutablesGitRev self.cardano-sl-node;
    cardano-sl-explorer-static = justStaticExecutablesGitRev self.cardano-sl-explorer;
    cardano-report-server-static = justStaticExecutablesGitRev self.cardano-report-server;
    cardano-sl-faucet-static = justStaticExecutablesGitRev self.cardano-sl-faucet;
    cardano-sl-tools = justStaticExecutablesGitRev (overrideCabal super.cardano-sl-tools (drv: {
      # waiting on load-command size fix in dyld
      doCheck = ! pkgs.stdenv.isDarwin;
    }));
    # Undo configuration-nix.nix change to hardcode security binary on darwin
    # This is needed for macOS binary not to fail during update system (using http-client-tls)
    # Instead, now the binary is just looked up in $PATH as it should be installed on any macOS
    x509-system = overrideDerivation super.x509-system (drv: {
      postPatch = ":";
    });

    # TODO: get rid of pthreads option once cryptonite 0.25 is released
    # DEVOPS-393: https://github.com/haskell-crypto/cryptonite/issues/193
    cryptonite = appendPatch (appendConfigureFlag super.cryptonite "--ghc-option=-optl-pthread") ./pkgs/cryptonite-segfault-blake.patch;

    # Due to https://github.com/input-output-hk/stack2nix/issues/56
    hfsevents = self.callPackage ./pkgs/hfsevents.nix { inherit (pkgs.darwin.apple_sdk.frameworks) Cocoa CoreServices; };
    mkDerivation = args: super.mkDerivation (args // {
      enableLibraryProfiling = enableProfiling;
      enableExecutableProfiling = enableProfiling;
      # Static linking for everything to work around
      # https://ghc.haskell.org/trac/ghc/ticket/14444
      # This will be the default in nixpkgs since
      # https://github.com/NixOS/nixpkgs/issues/29011
      enableSharedExecutables = false;
    } // optionalAttrs (args ? src) {
      src = localLib.cleanSourceTree args.src;
    });
  };
  benchmarkOverlay = self: super: {
    mkDerivation = args: super.mkDerivation (args // optionalAttrs (localLib.isCardanoSL args.pname) {
      # Enables building but not running of benchmarks for all
      # cardano-sl packages when enableBenchmarks argument is true.
      doBenchmark = true;
      configureFlags = (args.configureFlags or []) ++ ["--enable-benchmarks"];
    } // optionalAttrs (localLib.isBenchmark args) {
      # Provide a dummy installPhase for benchmark packages.
      installPhase = "mkdir -p $out";
    });
  };

  debugOverlay = self: super: {
    mkDerivation = args: super.mkDerivation (args // {
      # TODO: DEVOPS-355
      dontStrip = true;
      configureFlags = (args.configureFlags or []) ++ [ "--ghc-options=-g --disable-executable-stripping --disable-library-stripping" "--profiling-detail=toplevel-functions"];
    });
  };

  dontCheckOverlay = self: super: {
    mkDerivation = args: super.mkDerivation (args // {
      doCheck = false;
    });
  };

  metricOverlay = self: super: {
    mkDerivation = args: super.mkDerivation (args // {
      enablePhaseMetrics = true;
    });
  };

  cardanoPkgsBase = ((import ./pkgs { inherit pkgs; }).override {
    ghc = overrideDerivation pkgs.haskell.compiler.ghc822 (drv: {
      patches = drv.patches ++ [ ./ghc-8.0.2-darwin-rec-link.patch ];
    });
  });

  activeOverlays = [ requiredOverlay ]
      ++ optional enablePhaseMetrics metricOverlay
      ++ optional enableBenchmarks benchmarkOverlay
      ++ optional enableDebugging debugOverlay
      ++ optional forceDontCheck dontCheckOverlay;

  cardanoPkgs = builtins.foldl' (pkgs: overlay: pkgs.extend overlay) cardanoPkgsBase activeOverlays;
  connect = let
      walletConfigFile = ./custom-wallet-config.nix;
      walletConfig = if allowCustomConfig then (if builtins.pathExists walletConfigFile then import walletConfigFile else {}) else {};
    in
      args: pkgs.callPackage ./scripts/launch/connect-to-cluster (args // { inherit gitrev useStackBinaries; } // walletConfig );
  other = rec {
    testlist = innerClosePropagation [] [ cardanoPkgs.cardano-sl ];
    walletIntegrationTests = pkgs.callPackage ./scripts/test/wallet/integration { inherit gitrev useStackBinaries; };
    validateJson = pkgs.callPackage ./tools/src/validate-json {};
    demoCluster = pkgs.callPackage ./scripts/launch/demo-cluster { inherit gitrev useStackBinaries; };
    demoClusterDaedalusDev = pkgs.callPackage ./scripts/launch/demo-cluster { inherit gitrev useStackBinaries; disableClientAuth = true; numImportedWallets = 0; };
    demoClusterLaunchGenesis = pkgs.callPackage ./scripts/launch/demo-cluster {
      inherit gitrev useStackBinaries;
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
    mkDocker = { environment, connectArgs ? {} }: import ./docker.nix { inherit environment connect gitrev pkgs connectArgs; };
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
    };
    acceptanceTests = let
      acceptanceTest = pkgs.callPackage ./scripts/test/acceptance;
      mkTest = { environment, ...}: {
        full  = acceptanceTest { inherit environment; resume = false; };
        quick = acceptanceTest { inherit environment; resume = true; };
      };
    in localLib.forEnvironments mkTest;

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
