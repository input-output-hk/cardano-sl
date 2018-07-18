{ system ? builtins.currentSystem, target ? null }:
let
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
  overlay = self: super: {
    haskellPackages = ((import <stackage> { pkgs = super; }).lts-12_0
      # ontop of the LTS, inject the extra pacakges and source deps.
      { extraDeps = hsPkgs: (stack-pkgs.extraDeps hsPkgs
                          // stack-pkgs.packages hsPkgs)
                          // iserv-pkgs; }).override
        { overrides = self: super:
          # global overrides (effect haskellPackages, as well as buildHaskellPackages)
          { libiserv = super.libiserv.override { flags = { network = true; }; }; }; }; };

  # configure out general nix setup.
  # 
  config = {
    allowUnsupportedSystem = true;

    wine.build = "wine64";

    packageOverrides = ps: with ps; rec {
      # use the pre-built rocksdb.
      # We seem to be unable to actually
      # build rocksdb with mingw64/gcc7
      # without producing a partial dud.
      #
      # Using the pre-built rocksdb, also
      # means we do not need the gcc7 hack
      # in our nixpkgs to allow mingw with
      # libwinpthreads.
      rocksdb = buildPackages.callPackage ./rocksdb-prebuilt.nix {};

      # on windows we have this habit of putting libraries
      # into `bin`, wheras on unix it's usually `lib`. For
      # this confuses nix easily. So we'll just move the
      # .dll's from `bin` into `$out/lib`. Such that they
      # are trivially found.
      openssl = ps.openssl.overrideAttrs (drv: {
        postInstall = with ps.stdenv; drv.postInstall + lib.optionalString hostPlatform.isWindows ''
          cp $bin/bin/*.dll $out/lib/
        '';
      });
      mfpr = ps.mfpr.overrideAttrs (drv: {
        configureFlags = with ps.stdenv; (drv.configureFlags or []) ++ lib.optional hostPlatform.isWindows "--enable-static --disable-shared";
      });
      libmpc = ps.libmpc.overrideAttrs (drv: {
        configureFlags = with ps.stdenv; (drv.configureFlags or []) ++ lib.optional hostPlatform.isWindows "--enable-static --disable-shared";
      });

      haskell = lib.recursiveUpdate ps.haskell {
        lib = ps.haskell.lib // (with ps.haskell.lib; {
          # sanity
          addExtraLibrary'  = ls: drv: addExtraLibrary drv ls;
          addBuildDepends'  = ds: drv: addBuildDepends drv ds;
          appendBuildFlags' = fs: drv: appendBuildFlags drv fs;
          overrideCabal'    = os: drv: overrideCabal drv os;
          addBuildTools'    = ts: drv: addBuildTools drv ts;
          addPreBuild'      = x: drv:  overrideCabal drv (drv: { preBuild  = (drv.preBuild or  "") + x; });
          addPostBuild'     = x: drv:  overrideCabal drv (drv: { postBuild = (drv.postBuild or "") + x; });
          dropRevision'     = drv: overrideCabal drv (_: { revision = null; editedCabalFile = null; });
        });

        compiler.ghc843 = (ps.haskell.compiler.ghc843.override {
          ghcFlavour = if ps.stdenv.targetPlatform == ps.stdenv.hostPlatform then "quick" else "perf-cross-ncg";
          enableShared = false;
          enableIntegerSimple = false;
        }).overrideAttrs (drv: {
          dontStrip = true;
          hardeningDisable = [ "stackprotector" "format" ];
          patches = (drv.patches or []) ++ [
            ./move-iserv-8.4.2.patch
            ./hsc2hs-8.4.2.patch
            ./various-8.4.2.patch
            ./lowercase-8.4.2.patch
            ./cabal-exe-ext-8.4.2.patch
            ./dll-loader-8.4.2.patch
            ./outputtable-assert-8.4.2.patch
          ];
          postPatch = (drv.postPath or "") + ''
          autoreconf
          ''; 
        });
        packages.ghc843 = (ps.haskell.packages.ghc843.override {
          overrides = self: super: rec {
            mkDerivation = drv: super.mkDerivation (drv // {
              enableLibraryProfiling = false;
              enableSharedLibraries = false;
              enableSharedExecutables = false;
            });
          };
        });
      };
    };
  };

  # Combine the Overlay, and Config to produce
  # our package set.
  pkgs = import <nixpkgs> ({
    inherit system;
    overlays = [ overlay ];
    config = config;
    crossSystem = (import <nixpkgs/lib>).systems.examples.mingwW64;
  } // (if target != null
        then { crossSystem = { win64   = (import <nixpkgs/lib>).systems.examples.mingwW64;
                               macOS   = abort "macOS target not available";
                               rpi     = abort "Raspberry Pi target not available";
                               ios     = abort "iOS target not available";
                               android = abort "Android target not available"; }."${target}"; }
        else {}));

in with pkgs.haskellPackages;
with pkgs.haskell.lib;
pkgs.haskellPackages.override rec {
  # note: we want `haskellPackages` here, as that is the one
  #       we provide in the overlay(!)
  buildHaskellPackages = pkgs.buildPackages.haskellPackages;
  overrides = self: super: rec {

    # Logic to run TH via an external interpreter (64bit windows via wine64)
  doTemplateHaskellMingw32 = pkg: with pkgs.haskell.lib; let
    buildTools = [ buildHaskellPackages.iserv-proxy pkgs.buildPackages.winePackages.minimal ];
    buildFlags = map (opt: "--ghc-option=" + opt) [
      "-fexternal-interpreter"
      "-pgmi" "${buildHaskellPackages.iserv-proxy}/bin/iserv-proxy"
      "-opti" "127.0.0.1" "-opti" "$PORT"
      # TODO: this should be automatically injected based on the extraLibrary.
      "-L${pkgs.windows.mingw_w64_pthreads}/lib"
    ];
    preBuild = ''
      PORT=$((5000 + $RANDOM % 5000))
      echo "---> Starting remote-iserv on port $PORT"
      WINEPREFIX=$TMP ${pkgs.buildPackages.winePackages.minimal}/bin/wine64 ${self.remote-iserv}/bin/remote-iserv.exe tmp $PORT &
      sleep 60 # wait for wine to fully boot up...
      echo "---| remote-iserv should have started on $PORT"
      RISERV_PID=$!
    '';
    postBuild = ''
      echo "---> killing remote-iserv..."
      kill $RISERV_PID
    ''; in
    appendBuildFlags' buildFlags
     (addBuildDepends' [ self.remote-iserv ]
      (addExtraLibrary' pkgs.windows.mingw_w64_pthreads
       (addBuildTools' buildTools
        (addPreBuild' preBuild
         (addPostBuild' postBuild pkg)))));

  # how to perform TH for different host platforms.
  doTemplateHaskell = pkg:
    with pkgs.stdenv;
      if hostPlatform.isWindows
      then doTemplateHaskellMingw32 pkg
      else assert buildPlatform == hostPlatform; pkg;

  addGitRev = subject: subject.overrideAttrs (drv: { GITREV = "blahblahblah"; });
  doTemplateHaskellVerbose = pkg: with pkgs.haskell.lib; let
    buildTools = [ buildHaskellPackages.iserv-proxy pkgs.buildPackages.winePackages.minimal ];
    buildFlags = map (opt: "--ghc-option=" + opt) [
      "-fexternal-interpreter"
      "-pgmi" "${buildHaskellPackages.iserv-proxy}/bin/iserv-proxy"
      "-opti" "127.0.0.1" "-opti" "$PORT" "-opti" "-v" #"-xc" "+RTS" "-Di"
      # TODO: this should be automatically injected based on the extraLibrary.
      "-L${pkgs.windows.mingw_w64_pthreads}/lib"
    ];
    preBuild = ''
      PORT=$((5000 + $RANDOM % 5000))
      echo "---> Starting remote-iserv on port $PORT"
      WINEPREFIX=$TMP ${pkgs.buildPackages.winePackages.minimal}/bin/wine64 ${self.remote-iserv}/bin/remote-iserv.exe tmp $PORT -v +RTS -Di &
      sleep 60 # wait for wine to fully boot up...
      echo "---| remote-iserv should have started on $PORT"
      RISERV_PID=$!
    '';
    postBuild = ''
      echo "---> killing remote-iserv..."
      kill $RISERV_PID
    ''; in
    appendBuildFlags' buildFlags
     (addBuildDepends' [ self.remote-iserv ]
      (addExtraLibrary' pkgs.windows.mingw_w64_pthreads
       (addBuildTools' buildTools
        (addPreBuild' preBuild
         (addPostBuild' postBuild pkg)))));

    # TODO: Why is `network` not properly propagated from `libiserv`?
    remote-iserv = with pkgs.haskell.lib; let pkg = addExtraLibrary super.remote-iserv self.network; in
      overrideCabal (addBuildDepends pkg [ pkgs.windows.mingw_w64_pthreads ]) (drv: {
        postInstall = ''
          cp ${pkgs.windows.mingw_w64_pthreads}/bin/libwinpthread-1.dll $out/bin/
        '';
        buildFlags =  [ "--ghc-option=-debug" ];
      });
    streaming-commons     = appendPatch super.streaming-commons  ./streaming-commons-0.2.0.0.patch;
    cryptonite-openssl    = appendPatch super.cryptonite-openssl ./cryptonite-openssl-0.7.patch;
    x509-system           = appendPatch super.x509-system        ./x509-system-1.6.6.patch;
    conduit               = appendPatch super.conduit            ./conduit-1.3.0.2.patch;
    file-embed-lzma       = appendPatch super.file-embed-lzma    ./file-embed-lzma-0.patch;
    
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
    cardano-sl-wallet-new = addBuildDepends
                              (doTemplateHaskell (addGitRev super.cardano-sl-wallet-new))
                              # TODO: why do we need to explicity addBuilDepends his?
                              #       it should have been propagated from
                              #       rocksdb-haskell-ng?
                              [ pkgs.rocksdb ];

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

    rocksdb-haskell-ng    = appendConfigureFlag super.rocksdb-haskell-ng "--ghc-options=-v3";
  };
} // { pkgs-x = pkgs; }
