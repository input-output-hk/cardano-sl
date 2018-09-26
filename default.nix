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
, allowCustomConfig ? true
}:

with pkgs.lib;
with pkgs.haskell.lib;

let
  addGitRev = subject:
    subject.overrideAttrs (
      drv: {
        GITREV = gitrev;
        librarySystemDepends = (drv.librarySystemDepends or []) ++ [ pkgs.git ];
        executableSystemDepends = (drv.executableSystemDepends or []) ++ [ pkgs.git ];
      }
    );
  addRealTimeTestLogs = drv: overrideCabal drv (attrs: {
    testTarget = "--log=test.log || (sleep 10 && kill $TAILPID && false)";
    preCheck = ''
      mkdir -p dist/test
      touch dist/test/test.log
      tail -F dist/test/test.log &
      export TAILPID=$!
    '';
    postCheck = ''
      sleep 10
      kill $TAILPID
    '';
  });
  # Enables building but not running of benchmarks when
  # enableBenchmarks argument is true.
  buildWithBenchmarks = drv: if enableBenchmarks
    then doBenchmark (appendConfigureFlag drv "--enable-benchmarks")
    else drv;

  cardanoPkgs = ((import ./pkgs { inherit pkgs; }).override {
    ghc = overrideDerivation pkgs.haskell.compiler.ghc822 (drv: {
      patches = drv.patches ++ [ ./ghc-8.0.2-darwin-rec-link.patch ];
    });
    overrides = self: super: {
      srcroot = ./.;
      cardano-sl-core = overrideCabal super.cardano-sl-core (drv: {
        configureFlags = (drv.configureFlags or []) ++ [
          "-f-asserts"
        ];
      });

      cardano-sl = overrideCabal (buildWithBenchmarks super.cardano-sl) (drv: {
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

      cardano-sl-networking = buildWithBenchmarks super.cardano-sl-networking;
      cardano-sl-block-bench = buildWithBenchmarks super.cardano-sl-block-bench;
      cardano-sl-explorer = buildWithBenchmarks super.cardano-sl-explorer;
      cardano-sl-wallet-static = justStaticExecutables super.cardano-sl-wallet;
      cardano-sl-client = addRealTimeTestLogs super.cardano-sl-client;
      cardano-sl-generator = addRealTimeTestLogs super.cardano-sl-generator;
      # cardano-sl-auxx = addGitRev (justStaticExecutables super.cardano-sl-auxx);
      cardano-sl-auxx = addGitRev (justStaticExecutables super.cardano-sl-auxx);
      cardano-sl-node = addGitRev super.cardano-sl-node;
      cardano-sl-wallet-new = addGitRev super.cardano-sl-wallet-new;
      cardano-sl-wallet-new-static = addGitRev (justStaticExecutables (buildWithBenchmarks super.cardano-sl-wallet-new));
      cardano-sl-tools = addGitRev (justStaticExecutables (overrideCabal super.cardano-sl-tools (drv: {
        # waiting on load-command size fix in dyld
        doCheck = ! pkgs.stdenv.isDarwin;
      })));

      cardano-sl-node-static = justStaticExecutables self.cardano-sl-node;
      cardano-sl-explorer-static = addGitRev (justStaticExecutables self.cardano-sl-explorer);
      cardano-report-server-static = justStaticExecutables self.cardano-report-server;
      cardano-sl-faucet-static = addGitRev (justStaticExecutables self.cardano-sl-faucet);

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
        src = let
           cleanSourceFilter = with pkgs.stdenv;
             name: type: let baseName = baseNameOf (toString name); in ! (
               # Filter out .git repo
               (type == "directory" && baseName == ".git") ||
               # Filter out editor backup / swap files.
               lib.hasSuffix "~" baseName ||
               builtins.match "^\\.sw[a-z]$" baseName != null ||
               builtins.match "^\\..*\\.sw[a-z]$" baseName != null ||

               # Filter out locally generated/downloaded things.
               baseName == "dist" ||

               # Filter out the files which I'm editing often.
               lib.hasSuffix ".nix" baseName ||
               # Filter out nix-build result symlinks
               (type == "symlink" && lib.hasPrefix "result" baseName)
             );

          in
            if (builtins.typeOf args.src) == "path"
              then builtins.filterSource cleanSourceFilter args.src
              else args.src or null;
      } // optionalAttrs enableDebugging {
        # TODO: DEVOPS-355
        dontStrip = true;
        configureFlags = (args.configureFlags or []) ++ [ "--ghc-options=-g --disable-executable-stripping --disable-library-stripping" "--profiling-detail=toplevel-functions"];
      } // optionalAttrs (forceDontCheck == true) {
        doCheck = false;
      });
    };
  });
  connect = let
      walletConfigFile = ./custom-wallet-config.nix;
      walletConfig = if allowCustomConfig then (if builtins.pathExists walletConfigFile then import walletConfigFile else {}) else {};
    in
      args: pkgs.callPackage ./scripts/launch/connect-to-cluster (args // { inherit gitrev; } // walletConfig );
  other = rec {
    validateJson = pkgs.callPackage ./tools/src/validate-json {};
    demoCluster = pkgs.callPackage ./scripts/launch/demo-cluster { inherit gitrev; };
    demoClusterDaedalusDev = pkgs.callPackage ./scripts/launch/demo-cluster { inherit gitrev; disableClientAuth = true; numImportedWallets = 0; };
    demoClusterLaunchGenesis = pkgs.callPackage ./scripts/launch/demo-cluster {
      inherit gitrev;
      launchGenesis = true;
      configurationKey = "testnet_full";
      runWallet = false;
    };
    shellcheckTests = pkgs.callPackage ./scripts/test/shellcheck.nix { src = ./.; };
    swaggerSchemaValidation = pkgs.callPackage ./scripts/test/wallet/swaggerSchemaValidation.nix { inherit gitrev; };
    walletIntegrationTests = pkgs.callPackage ./scripts/test/wallet/integration { inherit gitrev; };
    buildWalletIntegrationTests = pkgs.callPackage ./scripts/test/wallet/integration/build-test.nix { inherit walletIntegrationTests pkgs; };
    cardano-sl-explorer-frontend = (import ./explorer/frontend {
      inherit system config gitrev pkgs;
      cardano-sl-explorer = cardanoPkgs.cardano-sl-explorer-static;
    });
    makeFaucetFrontend = pkgs.callPackage ./faucet/frontend;

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
        wallet = connect {};
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
