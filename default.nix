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

  stack-pkgs = import ./stack-pkgs.nix;

  # We use some customized libiserv/remote-iserv/iserv-proxy
  # instead of the ones provided by ghc. This is mostly due
  # to being able to hack on them freely as needed.
  #
  # iserv is only relevant for template-haskell execution in
  # a cross compiling setup.
  iserv-pkgs = {
    libiserv = ./libiserv-8.5;
    remote-iserv = ./remote-iserv-8.5;
    iserv-proxy = ./iserv-proxy-8.5;
  };

  # Build the LTS-12 overlay.
  #
  # TODO: have this be produced in the `stack-pkgs.nix` file
  #       that way we could pick the proper `lts-12_0` based
  #       on the stack.yaml file, instead of hand-injecting
  #       it here.
  hackage = import localLib.fetchHackage;
  haskell = import localLib.fetchHaskell hackage;
  packageOverlay = self: super: with self.haskell.lib; {
    # the lts-XX_Y is essentially the same as what pkgs/default.nix returns.
    haskellPackages = ((import localLib.fetchStackage { pkgs = self; inherit hackage haskell; }).lts-12_2
      # ontop of the LTS, inject the extra pacakges and source deps.
      { extraDeps = hsPkgs: (stack-pkgs.extraDeps hsPkgs
                          // stack-pkgs.packages hsPkgs)
                          // iserv-pkgs; }).override
        { overrides = self: super:
          # global overrides (effect haskellPackages, as well as buildHaskellPackages)
          { libiserv = super.libiserv.override { flags = { network = true; }; };
            # break infinite recursion due to tests depending on packages that use
            # the test-packages to test themselves.
            nanospec = dontCheck super.nanospec;
            test-framework = dontCheck super.test-framework;
            tasty = dontCheck super.tasty;
            integer-logarithms = dontCheck super.integer-logarithms;
            attoparsec = dontCheck super.attoparsec;
            text = dontCheck super.text;
            clock = dontCheck super.clock;
            scientific = dontCheck super.scientific;
            statistics = dontCheck super.statistics;
            dlist = dontCheck super.dlist;
            hspec = dontCheck (super.hspec.override { hsPkgs = { stringbuilder = dontCheck self.stringbuilder; }; });
            # missing: bytestring-handle, hashable-time
            aeson = dontCheck super.aeson;
            mwc-random = dontCheck super.mwc-random;
            vector-builder = dontCheck super.vector-builder;
            tar = dontCheck super.tar;
            system-filepath = dontCheck super.system-filepath;
            network-transport-inmemory = dontCheck super.network-transport-inmemory;
            concurrent-extra = dontCheck super.concurrent-extra;
            xmlgen = dontCheck super.xmlgen;
            http-date = dontCheck super.http-date;
            mockery = dontCheck super.mockery;
            DRBG = dontCheck super.DRBG;
            math-functions = dontCheck super.math-functions;

            # case sensitivity issue?
            rocksdb-haskell-ng = dontCheck super.rocksdb-haskell-ng;
            megaparsec = dontCheck super.megaparsec;

            Cabal = appendPatches super.Cabal
              [ # allow a script/application be specified that wraps the test executable that's run.
                ./Cabal2201-allow-test-wrapper.patch
                # Cabal is lacking Semigroup-Monoid-Proposal support in the tests/HackageTests.hs
                ./Cabal2201-SMP-test-fix.patch
                # as we push a lot of arguments to the `Setup` we might need @file support.
                ./Cabal2201-response-file-support.patch
                # > hackage-tests: /homeless-shelter/.cabal/config: openBinaryFile: does not exist (No such file or directory)
                ./Cabal2201-no-hackage-tests.patch
              ];

            # requires phantomJS
            wai-cors = dontCheck super.wai-cors;

            # These are missing `doctest`
            network = dontCheck super.network;
            distributive = dontCheck super.distributive;
            comonad = dontCheck super.comonad;
            iproute = dontCheck super.iproute;
            semigroupoids = dontCheck super.semigroupoids;
            systemd = dontCheck super.systemd;
            unix-time = dontCheck super.unix-time;
            dns = dontCheck super.dns;
            http-types = dontCheck super.http-types;
            kademlia = dontCheck super.kademlia;
            wai-logger = dontCheck super.wai-logger;
            http-api-data = dontCheck super.http-api-data;
            vector-algorithms = dontCheck super.vector-algorithms;
            http2 = dontCheck super.http2;
            servant = dontCheck super.servant;
            fmt = dontCheck super.fmt;
            lens = dontCheck super.lens;
            servant-server = dontCheck super.servant-server;
            ed25519 = dontCheck super.ed25519;
            universum = dontCheck super.universum;
            o-clock = dontCheck super.o-clock;
            lens-action = dontCheck super.lens-action;
            lens-aeson = dontCheck super.lens-aeson;
            trifecta = dontCheck super.trifecta;
            swagger2 = dontCheck super.swagger2;
            servant-swagger = dontCheck super.servant-swagger;
            aeson-diff = dontCheck super.aeson-diff;
            loc = dontCheck super.loc;
            ip = dontCheck super.ip;

            # this will fail with doTemplateHaskell.
            # due to some object-file reloading issue
            # in iserv.
            vector = dontCheck super.vector;

            hspec-discover        = dontCheck super.hspec-discover;
            hspec-core            = dontCheck super.hspec-core;
            # most of these fail due to depending on hspec-discover
            # at test-build time.
            base-orphans          = addBuildTools super.base-orphans          [ self.buildPackages.hspec-discover ];
            safe-exceptions       = addBuildTools super.safe-exceptions       [ self.buildPackages.hspec-discover ];
            wai                   = addBuildTools super.wai                   [ self.buildPackages.hspec-discover ];
            constraints           = addBuildTools super.constraints           [ self.buildPackages.hspec-discover ];
            unliftio              = addBuildTools super.unliftio              [ self.buildPackages.hspec-discover ];
            streaming-commons     = addBuildTools super.streaming-commons     [ self.buildPackages.hspec-discover ];
            # this one hits a bug in iserv! See https://ghc.haskell.org/trac/ghc/ticket/15481
            th-abstraction        = addBuildTools (dontCheck super.th-abstraction) [ self.buildPackages.hspec-discover ];
            base-compat-batteries = addBuildTools super.base-compat-batteries [ self.buildPackages.hspec-discover ];
            word8                 = addBuildTools super.word8                 [ self.buildPackages.hspec-discover ];
            fast-logger           = addBuildTools super.fast-logger           [ self.buildPackages.hspec-discover ];
            logging-facade        = addBuildTools super.logging-facade        [ self.buildPackages.hspec-discover ];
            newtype-generics      = addBuildTools super.newtype-generics      [ self.buildPackages.hspec-discover ];
            string-conversions    = addBuildTools super.string-conversions    [ self.buildPackages.hspec-discover ];
            bifunctors            = addBuildTools super.bifunctors            [ self.buildPackages.hspec-discover ];
            deriving-compat       = addBuildTools super.deriving-compat       [ self.buildPackages.hspec-discover ];
            generic-deriving      = addBuildTools super.generic-deriving      [ self.buildPackages.hspec-discover ];
            invariant             = addBuildTools super.invariant             [ self.buildPackages.hspec-discover ];
            adjunctions           = addBuildTools super.adjunctions           [ self.buildPackages.hspec-discover ];
            th-utilities          = addBuildTools super.th-utilities          [ self.buildPackages.hspec-discover ];
            HTF                   = addBuildTools (dontCheck super.HTF)       [ self.buildPackages.cpphs          ];
            servant-client-core   = addBuildTools super.servant-client-core   [ self.buildPackages.hspec-discover ];
            serokell-util         = addBuildTools super.serokell-util         [ self.buildPackages.hspec-discover ];
            wai-extra             = addBuildTools super.wai-extra             [ self.buildPackages.hspec-discover ];
            neat-interpolation    = addBuildTools super.neat-interpolation    [ self.buildPackages.HTF            ];
            conduit-extra         = addBuildTools super.conduit-extra         [ self.buildPackages.hspec-discover ];
            yaml                  = addBuildTools super.yaml                  [ self.buildPackages.hspec-discover ];
            log-warper            = addBuildTools super.log-warper            [ self.buildPackages.hspec-discover ];
            cardano-report-server = addBuildTools super.cardano-report-server [ self.buildPackages.hspec-discover ];
            servant-client        = addBuildTools super.servant-client        [ self.buildPackages.hspec-discover ];
            cardano-sl-util       = addBuildTools super.cardano-sl-util       [ self.buildPackages.hspec-discover ];
            cardano-sl-binary     = addBuildTools super.cardano-sl-binary     [ self.buildPackages.hspec-discover ];
            cardano-sl-crypto     = addBuildTools super.cardano-sl-crypto     [ self.buildPackages.hspec-discover ];
            cardano-sl-core       = addBuildTools super.cardano-sl-core       [ self.buildPackages.hspec-discover ];
            cardano-sl-chain      = addBuildTools super.cardano-sl-chain      [ self.buildPackages.hspec-discover ];
            cardano-sl-networking = addBuildTools super.cardano-sl-networking [ self.buildPackages.hspec-discover ];
            cardano-sl-client     = addBuildTools super.cardano-sl-client     [ self.buildPackages.hspec-discover ];
            cardano-sl-generator  = addBuildTools super.cardano-sl-generator  [ self.buildPackages.hspec-discover ];
            cardano-sl-wallet     = addBuildTools super.cardano-sl-wallet     [ self.buildPackages.hspec-discover ];
            cardano-sl-auxx       = addBuildTools super.cardano-sl-auxx       [ self.buildPackages.hspec-discover ];
            cardano-sl-explorer   = addBuildTools super.cardano-sl-explorer   [ self.buildPackages.hspec-discover ];
            servant-docs          = addBuildTools super.servant-docs          [ self.buildPackages.hspec-discover ];
            servant-quickcheck    = addBuildTools super.servant-quickcheck    [ self.buildPackages.hspec-discover ];
            # can't check cardano-sl due to an iserv bug
            cardano-sl            = addBuildTools (dontCheck super.cardano-sl) [ self.buildPackages.hspec-discover ];
            fclabels              = dontCheck super.fclabels;

            # test/sample/bar\baz: openBinaryFile: does not exist (No such file or directory)
            file-embed = dontCheck super.file-embed;

            stm-delay = dontCheck super.stm-delay; # https://hydra.iohk.io/build/193506/nixlog/14
            hspec-expectations-pretty-diff = dontCheck super.hspec-expectations-pretty-diff; # https://hydra.iohk.io/build/193533/nixlog/1
            simple-sendfile       = dontCheck super.simple-sendfile; # the test depends on `unix` and thus fails to test on windows.
          }; }; };
in
{ system ? builtins.currentSystem
, config ? import ./config.nix
, gitrev ? localLib.commitIdFromGitRepo ./.git
, buildId ? null
# for what target are we building. Currently the only supported is "win64".
, target ? null
, allowCustomConfig ? true
, n ? 0 }:
let
  # this is some hack to append spaces to the configureFlags of the
  # default derivation in GHC.  This allows us to force a rebuild
  # of all haskell packages.
  spaces = let repeat = n: c: c + (if n == 0 then "" else repeat (n - 1) c); in repeat n " ";
  spacedConfig = config spaces;

  # Combine the Overlay, and Config to produce
  # our package set.
  pkgs = import localLib.fetchNixPkgs ({
    inherit system;
    overlays = [ jemallocOverlay packageOverlay ];
    config = spacedConfig;
  } // (if target != null
        then { crossSystem = { win64   = (import localLib.fetchNixPkgs {}).lib.systems.examples.mingwW64;
                               macOS   = abort "macOS target not available";
                               rpi     = abort "Raspberry Pi target not available";
                               ios     = abort "iOS target not available";
                               android = abort "Android target not available"; }."${target}"; }
        else {}));
  binSuffix = if pkgs.stdenv.hostPlatform.isWindows then ".exe" else "";
  maybeViaWine = if pkgs.stdenv.hostPlatform.isWindows then ''WINEPREFIX=$TMP ${pkgs.buildPackages.winePackages.minimal}/bin/wine64'' else "";

in with pkgs.haskellPackages;
let iohkPkgs =
let
  # note: we want `haskellPackages` here, as that is the one
  #       we provide in the overlay(!)
  buildHaskellPackages = pkgs.buildPackages.haskellPackages;

  justStaticExecutablesGitRev = import ./scripts/set-git-rev {
    inherit pkgs gitrev;
    inherit (buildHaskellPackages) ghc;
  };
  addRealTimeTestLogs = drv: overrideCabal drv (attrs: {
    testTarget = "--show-details=streaming";
  });

  cardanoPkgs = (with pkgs.haskell.lib;
        pkgs.haskellPackages.override (old:
        let new_overrides = self: super: rec {

          inherit (import ./lib-mingw32.nix { inherit pkgs self; }) doTemplateHaskell appendPatchMingw forceCheck;

          # TODO: Why is `network` not properly propagated from `libiserv`?
          remote-iserv = with pkgs.haskell.lib; let pkg = addExtraLibrary super.remote-iserv self.network; in
            overrideCabal (addBuildDepends pkg [ pkgs.windows.mingw_w64_pthreads ]) (drv: {
              postInstall = ''
                cp ${pkgs.windows.mingw_w64_pthreads}/bin/libwinpthread-1.dll $out/bin/
              '';
              buildFlags =  [ "--ghc-option=-debug" ];
            });
          streaming-commons     = appendPatchMingw super.streaming-commons  ./streaming-commons-0.2.0.0.patch;
          cryptonite-openssl    = appendPatchMingw super.cryptonite-openssl ./cryptonite-openssl-0.7.patch;
          # Undo configuration-nix.nix change to hardcode security binary on darwin
          # This is needed for macOS binary not to fail during update system (using http-client-tls)
          # Instead, now the binary is just looked up in $PATH as it should be installed on any macOS

          x509-system           = appendPatchMingw super.x509-system        ./x509-system-1.6.6.patch;#) (drv: { postPatch = ":"; });
          conduit               = appendPatchMingw super.conduit            ./conduit-1.3.0.2.patch;
          double-conversion     = appendPatchMingw super.double-conversion  ./double-conversion-2.0.2.0.patch;
          file-embed-lzma       = doTemplateHaskell (appendPatchMingw super.file-embed-lzma    ./file-embed-lzma-0.patch);

          ether                 = doTemplateHaskell super.ether;
          generics-sop          = doTemplateHaskell (dontCheck super.generics-sop);
          th-lift-instances     = doTemplateHaskell super.th-lift-instances;
          math-functions        = doTemplateHaskell super.math-functions;
          wreq                  = doTemplateHaskell super.wreq;
          swagger2              = doTemplateHaskell super.swagger2;
          log-warper            = doTemplateHaskell super.log-warper;
          th-orphans            = doTemplateHaskell super.th-orphans;
          wai-app-static        = doTemplateHaskell super.wai-app-static;
          purescript-bridge     = doTemplateHaskell super.purescript-bridge;

          cardano-sl-util       = forceCheck (doTemplateHaskell super.cardano-sl-util);
          cardano-sl-auxx       = forceCheck (doTemplateHaskell super.cardano-sl-auxx);
          cardano-sl-crypto     = forceCheck (doTemplateHaskell super.cardano-sl-crypto);
          cardano-sl-crypto-test= forceCheck (doTemplateHaskell super.cardano-sl-crypto-test);
          cardano-sl-networking = forceCheck (doTemplateHaskell super.cardano-sl-networking);
          cardano-sl-core       = forceCheck (doTemplateHaskell super.cardano-sl-core);
          cardano-sl-core-test  = forceCheck (doTemplateHaskell super.cardano-sl-core-test);

          cardano-sl-chain      = forceCheck (doTemplateHaskell super.cardano-sl-chain);
          cardano-sl-chain-test = forceCheck (doTemplateHaskell super.cardano-sl-chain-test);

          cardano-sl-db         = forceCheck (doTemplateHaskell super.cardano-sl-db);
          cardano-sl-db-test    = forceCheck (doTemplateHaskell super.cardano-sl-db-test);
          cardano-sl-infra      = forceCheck (doTemplateHaskell super.cardano-sl-infra);
          # don't force-check cardano-sl; it's broken with iserv; see above.
          cardano-sl            = doTemplateHaskell super.cardano-sl;

          fclabels              = doTemplateHaskell super.fclabels;
          servant-docs          = doTemplateHaskell super.servant-docs;
          wai-websockets        = doTemplateHaskell super.wai-websockets;
          servant-swagger-ui    = doTemplateHaskell super.servant-swagger-ui;
          servant-swagger-ui-redoc = doTemplateHaskell super.servant-swagger-ui-redoc;
          cardano-sl-client     = forceCheck (doTemplateHaskell super.cardano-sl-client);
          cardano-sl-generator  = forceCheck (doTemplateHaskell super.cardano-sl-generator);
          cardano-sl-wallet     = forceCheck (doTemplateHaskell super.cardano-sl-wallet);

          cardano-sl-wallet-new = (doTemplateHaskell super.cardano-sl-wallet-new).overrideAttrs( old: { NIX_DEBUG = 1; });
          cardano-sl-infra-test = forceCheck (doTemplateHaskell super.cardano-sl-infra-test);
          cardano-sl-explorer   = forceCheck (doTemplateHaskell super.cardano-sl-explorer);
          cardano-sl-binary     = forceCheck (doTemplateHaskell super.cardano-sl-binary);

          trifecta              = doTemplateHaskell super.trifecta;
          cardano-sl-tools      = doTemplateHaskell super.cardano-sl-tools;
          hedgehog              = doTemplateHaskell super.hedgehog;
          th-abstraction        = doTemplateHaskell super.th-abstraction;
          th-expand-syns        = doTemplateHaskell super.th-expand-syns;
          file-embed            = doTemplateHaskell super.file-embed;
          QuickCheck            = doTemplateHaskell (dontCheck super.QuickCheck);
          optparse-applicative  = doTemplateHaskell super.optparse-applicative;
          quickcheck-text       = doTemplateHaskell super.quickcheck-text;

          th-reify-many         = doTemplateHaskell super.th-reify-many;
          vector                = doTemplateHaskell super.vector;
          tasty-th              = doTemplateHaskell super.tasty-th;
          lifted-async          = doTemplateHaskell super.lifted-async;
          vector-th-unbox       = doTemplateHaskell super.vector-th-unbox;

          th-lift               = doTemplateHaskell super.th-lift;
          microlens-th          = doTemplateHaskell super.microlens-th;
          micro-recursion-schemes = doTemplateHaskell super.micro-recursion-schemes;
          bifunctors            = doTemplateHaskell super.bifunctors;
          deriving-compat       = doTemplateHaskell super.deriving-compat;
          generic-deriving      = doTemplateHaskell super.generic-deriving;
          uri-bytestring        = doTemplateHaskell super.uri-bytestring;
          invariant             = doTemplateHaskell super.invariant;
          th-utilities          = doTemplateHaskell super.th-utilities;
          HTF                   = doTemplateHaskell super.HTF;
          safecopy              = doTemplateHaskell super.safecopy;
          yaml                  = doTemplateHaskell super.yaml;
          neat-interpolation    = doTemplateHaskell super.neat-interpolation;
          monad-par             = doTemplateHaskell super.monad-par;
          statistics            = doTemplateHaskell super.statistics;
          edit-distance-vector  = doTemplateHaskell super.edit-distance-vector;
          ixset-typed           = doTemplateHaskell super.ixset-typed;
          sqlite-simple         = doTemplateHaskell super.sqlite-simple;

          cassava               = super.cassava.override            { flags = { bytestring--lt-0_10_4 = false; }; };
          time-locale-compat    = super.time-locale-compat.override { flags = { old-locale = false; }; };

          # TODO: Why is this not propagated properly? Only into the buildHasekllPackages?
          libiserv              = super.libiserv.override           { flags = { network = true; }; };

          # This should be stubbed out in <stackage/package-set.nix>; however lts-12 fails to list Win32
          # as one of the windows packages as such just fails.
          Win32 = null;

          cardano-sl-node-static = justStaticExecutablesGitRev self.cardano-sl-node;
          cardano-sl-explorer-static = justStaticExecutablesGitRev self.cardano-sl-explorer;
          cardano-report-server-static = justStaticExecutablesGitRev self.cardano-report-server;
       }; in {
         # we need this rather convoluted overriding here to preserve prior overrides.
         # see https://github.com/NixOS/nixpkgs/issues/26561#issuecomment-397350884
         overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: {})) new_overrides;
       })
  );
  connect = let
      walletConfigFile = ./custom-wallet-config.nix;
      walletConfig = if allowCustomConfig then (if builtins.pathExists walletConfigFile then import walletConfigFile else {}) else {};
    in
      args: pkgs.callPackage ./scripts/launch/connect-to-cluster (args // { inherit iohkPkgs; } // walletConfig );
  other = rec {
    walletIntegrationTests = pkgs.callPackage ./scripts/test/wallet/integration { inherit gitrev; };
    validateJson = pkgs.callPackage ./tools/src/validate-json {};
    demoCluster = pkgs.callPackage ./scripts/launch/demo-cluster { inherit gitrev; };
    demoClusterLaunchGenesis = pkgs.callPackage ./scripts/launch/demo-cluster {
      inherit gitrev;
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
    all-cardano-sl = pkgs.buildEnv {
      name = "all-cardano-sl";
      paths = builtins.attrValues (pkgs.lib.filterAttrs (name: drv: localLib.isCardanoSL name) cardanoPkgs);
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
      ${pkgs.lib.optionalString (buildId != null) "echo ${buildId} > build-id"}
      echo ${gitrev} > commit-id
      echo ${version} > version

      cp --no-preserve=mode -R ${cardano-sl-config}/lib config
      cp ${cardano-sl-config}/log-configs/daedalus.yaml $out/config/log-config-prod.yaml
      cp ${cardanoPkgs.cardano-sl-tools}/bin/cardano-launcher${binSuffix} bin
      cp ${cardanoPkgs.cardano-sl-tools}/bin/cardano-x509-certificates${binSuffix} bin
      cp ${cardanoPkgs.cardano-sl-wallet-new}/bin/cardano-node${binSuffix} bin

      ${pkgs.lib.optionalString pkgs.stdenv.hostPlatform.isWindows ''
      cp ${pkgs.rocksdb}/lib/*.dll bin
      cp ${pkgs.openssl.bin}/bin/*.dll bin
      ''}

      # test that binaries exit with 0
      ${maybeViaWine} ./bin/cardano-node${binSuffix} --help > /dev/null
      HOME=$TMP ${maybeViaWine} ./bin/cardano-launcher${binSuffix} --help > /dev/null
    '';
    CardanoSL = pkgs.runCommand "CardanoSL.zip" { nativeBuildInputs = [ pkgs.buildPackages.zip ]; } ''
      mkdir $out
      cd $out

      mkdir daedalus
      cp ${./log-configs/daedalus.yaml} daedalus/log-config-prod.yaml
      cp ${./lib/configuration.yaml}    daedalus/configuration.yaml
      cp ${./lib}/*genesis*.json        daedalus/
      cp ${ps.cardano-sl-tools}/bin/cardano-launcher${binSuffix}          daedalus/
      cp ${ps.cardano-sl-tools}/bin/cardano-x509-certificates${binSuffix} daedalus/
      cp ${ps.cardano-sl-wallet-new}/bin/cardano-node${binSuffix}         daedalus/
      #  - Echo %APPVEYOR_BUILD_VERSION% > build-id
      echo "BAD_BUILD_VERSION" >                               daedalus/build-id
      echo "${gitrev}"         >                               daedalus/commit-id
      #  - Echo https://ci.appveyor.com/project/%APPVEYOR_ACCOUNT_NAME%/%APPVEYOR_PROJECT_SLUG%/build/%APPVEYOR_BUILD_VERSION% > ci-url
      echo "BAD_CI_URL"        >                               daedalus/ci-url
      cd daedalus
      zip $out/CardanoSL.zip *
      cd ..
      rm -fR daedalus
    '';
  };
in cardanoPkgs // other;
in iohkPkgs
