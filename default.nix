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
  packageOverlay = self: super: {
    # the lts-XX_Y is essentially the same as what pkgs/default.nix returns.
    haskellPackages = ((import localLib.fetchStackage { pkgs = super; inherit hackage haskell; }).lts-12_2
      # ontop of the LTS, inject the extra pacakges and source deps.
      { extraDeps = hsPkgs: (stack-pkgs.extraDeps hsPkgs
                          // stack-pkgs.packages hsPkgs)
                          // iserv-pkgs; }).override
        { overrides = self: super:
          # global overrides (effect haskellPackages, as well as buildHaskellPackages)
          { libiserv = super.libiserv.override { flags = { network = true; }; }; }; }; };
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
        pkgs.haskellPackages.override rec {
    overrides = self: super: rec {


    inherit (import ./lib-mingw32.nix { inherit pkgs self; }) doTemplateHaskell appendPatchMingw;

    addGitRev = subject: subject.overrideAttrs (drv: { GITREV = gitrev; });

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
    file-embed-lzma       = appendPatchMingw super.file-embed-lzma    ./file-embed-lzma-0.patch;
    
    ether                 = doTemplateHaskell super.ether;
    generics-sop          = doTemplateHaskell super.generics-sop;
    th-lift-instances     = doTemplateHaskell super.th-lift-instances;
    math-functions        = doTemplateHaskell super.math-functions;
    wreq                  = doTemplateHaskell super.wreq;
    swagger2              = doTemplateHaskell super.swagger2;
    log-warper            = doTemplateHaskell super.log-warper;
    th-orphans            = doTemplateHaskell super.th-orphans;
    wai-app-static        = doTemplateHaskell super.wai-app-static;

    cardano-sl-util       = doTemplateHaskell super.cardano-sl-util;
    cardano-sl-crypto     = doTemplateHaskell super.cardano-sl-crypto;
    cardano-sl-crypto-test= doTemplateHaskell super.cardano-sl-crypto-test;
    cardano-sl-networking = doTemplateHaskell super.cardano-sl-networking;
    cardano-sl-core       = doTemplateHaskell super.cardano-sl-core;
    cardano-sl-core-test  = doTemplateHaskell super.cardano-sl-core-test;
    
    cardano-sl-db         = doTemplateHaskell super.cardano-sl-db;
    cardano-sl-lrc        = doTemplateHaskell super.cardano-sl-lrc;
    cardano-sl-infra      = doTemplateHaskell super.cardano-sl-infra;
    cardano-sl-txp        = doTemplateHaskell super.cardano-sl-txp;
    cardano-sl-delegation = doTemplateHaskell super.cardano-sl-delegation;
    cardano-sl-update     = doTemplateHaskell super.cardano-sl-update;
    cardano-sl-ssc        = doTemplateHaskell super.cardano-sl-ssc;
    cardano-sl-block      = doTemplateHaskell super.cardano-sl-block;
    cardano-sl            = doTemplateHaskell super.cardano-sl;

    fclabels              = doTemplateHaskell super.fclabels;
    servant-docs          = doTemplateHaskell super.servant-docs;
    wai-websockets        = doTemplateHaskell super.wai-websockets;
    servant-swagger-ui    = doTemplateHaskell super.servant-swagger-ui;
    servant-swagger-ui-redoc = doTemplateHaskell super.servant-swagger-ui-redoc;
    cardano-sl-client     = doTemplateHaskell super.cardano-sl-client;
    cardano-sl-generator  = doTemplateHaskell super.cardano-sl-generator;
    cardano-sl-wallet     = doTemplateHaskell super.cardano-sl-wallet;
    cardano-sl-wallet-new = doTemplateHaskell super.cardano-sl-wallet-new;

    cardano-sl-sinbin     = doTemplateHaskell super.cardano-sl-sinbin;

    trifecta              = doTemplateHaskell super.trifecta;
    cardano-sl-tools      = doTemplateHaskell
                              (addGitRev
                                (super.cardano-sl-tools.override { flags = { for-installer = true; }; }));
    hedgehog              = doTemplateHaskell super.hedgehog;

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
  };
});
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
      walletIntegration = pkgs.callPackage ./scripts/test/wallet/integration/build-test.nix { inherit walletIntegrationTests pkgs; };
      swaggerSchemaValidation = pkgs.callPackage ./scripts/test/wallet/swaggerSchemaValidation.nix { inherit gitrev; };
    };
    cardano-sl-explorer-frontend = (import ./explorer/frontend {
      inherit system config gitrev pkgs;
      cardano-sl-explorer = cardanoPkgs.cardano-sl-explorer-static;
    });
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
    CardanoSL = pkgs.runCommand "CardanoSL.zip" { nativeBuildInputs = [ pkgs.buildPackages.zip ]; } ''
      mkdir $out
      cd $out

      mkdir daedalus
      cp ${./log-configs/daedalus.yaml} daedalus/log-config-prod.yaml
      cp ${./lib/configuration.yaml}    daedalus/configuration.yaml
      cp ${./lib}/*genesis*.json        daedalus/
      cp ${ps.cardano-sl-tools}/bin/cardano-launcher.exe          daedalus/
      cp ${ps.cardano-sl-tools}/bin/cardano-x509-certificates.exe daedalus/
      cp ${ps.cardano-sl-wallet-new}/bin/cardano-node.exe         daedalus/
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
