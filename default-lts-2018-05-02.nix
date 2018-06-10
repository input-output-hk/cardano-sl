let

  stack-pkgs = import ./stack-pkgs.nix;
  #haskell = import <haskell>;

  overlay = self: super: {
    haskellPackages = (import <stackage> { pkgs = super; }).lts-2018-05-02
      { extraDeps = hsPkgs: (stack-pkgs.extraDeps hsPkgs
                          // stack-pkgs.packages hsPkgs); };
  };

  config = {
    allowUnsupportedSystem = true;

    wine.build = "wine64";

    packageOverrides = ps: with ps; rec {
      rocksdb = ps.rocksdb.overrideAttrs (drv: { patches = (drv.patches or []) ++ [ ./rocksdb-5.11.patch ]; });
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

        compiler.ghc842 = (ps.haskell.compiler.ghc842.override {
          ghcCrossFlavour = "quick-cross-ncg";
          ghcFlavour = "quick";
          enableShared = false;
          enableIntegerSimple = false;
        }).overrideAttrs (drv: {
          dontStrip = true;
          hardeningDisable = [ "stackprotector" ];
          patches = (drv.patches or []) ++ [
            ./move-iserv-8.4.2.patch
            ./hsc2hs-8.4.2.patch
            ./various-8.4.2.patch
            ./lowercase-8.4.2.patch
            ./cabal-exe-ext-8.4.2.patch
          ];
          postPatch = (drv.postPath or "") + ''
          autoreconf
          ''; 
        });
        packages.ghc842 = (ps.haskell.packages.ghc842.override {
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

  pkgs = import <nixpkgs> {
    overlays = [ overlay ];
    config = config;
    crossSystem = (import <nixpkgs/lib>).systems.examples.mingwW64;
  };

in with pkgs.haskellPackages;
# pkgs.lib.mapAttrs (_: x: callPackage x {})
pkgs.haskellPackages.override rec {
  # note: we want `haskellPackages` here, as that is the one
  #       we provide in the overly(!)
  buildHaskellPackages = pkgs.buildPackages.haskellPackages;
  overrides = self: super: rec {

    # Logic to run TH via an external interpreter (64bit windows via wine64)
  doTemplateHaskell = pkg: with pkgs.haskell.lib; let
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

  addGitRev = subject: subject.overrideAttrs (drv: { GITREV = "blahblahblah"; });
  doSymlinkLibs = pkg: let targetPrefix = with pkgs.stdenv; lib.optionalString
    (hostPlatform != buildPlatform)
    "${hostPlatform.config}-";

    in pkgs.haskell.lib.overrideCabal pkg (drv: {
     preConfigure = ''
        echo "Patching dynamic library dependencies"
        # 1. Link all dylibs from 'dynamic-library-dirs's in package confs to $out/lib/links
        mkdir -p $out/lib/links
        for d in $(grep dynamic-library-dirs $packageConfDir/*|awk '{print $2}'); do
          for l in $d/*.dylib; do
            ln -s $l $out/lib/links/$(basename $l)
          done
        done
            
        # 2. Patch 'dynamic-library-dirs' in package confs to point to the symlink dir
        for f in $packageConfDir/*.conf; do
          sed -i "s,dynamic-library-dirs: .*,dynamic-library-dirs: $out/lib/links," $f
        done

        # 3. Recache package database
        ls $out/lib/links
        echo "Recaching..."
        ${targetPrefix}ghc-pkg --package-db="$packageConfDir" recache
    '' + (drv.preConfigure or"");
#    preConfigurePhases = [ "patchDynLibs" ] ++ (drv.preConfigurePhases or []);
  });
  doTemplateHaskellVerbose = pkg: with pkgs.haskell.lib; let
    buildTools = [ buildHaskellPackages.iserv-proxy pkgs.buildPackages.winePackages.minimal ];
    buildFlags = map (opt: "--ghc-option=" + opt) [
      "-fexternal-interpreter"
      "-pgmi" "${buildHaskellPackages.iserv-proxy}/bin/iserv-proxy"
      "-opti" "127.0.0.1" "-opti" "$PORT" "-opti" "-v"
      # TODO: this should be automatically injected based on the extraLibrary.
      "-L${pkgs.windows.mingw_w64_pthreads}/lib"
    ];
    preBuild = ''
      PORT=$((5000 + $RANDOM % 5000))
      echo "---> Starting remote-iserv on port $PORT"
      WINEPREFIX=$TMP ${pkgs.buildPackages.winePackages.minimal}/bin/wine64 ${self.remote-iserv}/bin/remote-iserv.exe tmp $PORT -v &
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

    #fetch a package candidate from hackage and return the cabal2nix expression.
    hackageCandidate = name: ver: args: self.callCabal2nix name (fetchTarball "https://hackage.haskell.org/package/${name}-${ver}/candidate/${name}-${ver}.tar.gz") args;

    # TODO: Why is `network` not properly propagated from `libiserv`?
    remote-iserv = with pkgs.haskell.lib; let pkg = addExtraLibrary super.remote-iserv self.network; in
      overrideCabal (addBuildDepends pkg [ pkgs.windows.mingw_w64_pthreads ]) (drv: {
        postInstall = ''
          cp ${pkgs.windows.mingw_w64_pthreads}/bin/libwinpthread-1.dll $out/bin/
          cp ${pkgs.gcc7-ng-libssp}/lib/libssp-0.dll $out/bin/
        '';
        buildFlags =  [ "--ghc-option=-debug" ];
      });
    streaming-commons = pkgs.haskell.lib.appendPatch super.streaming-commons ./streaming-commons-0.2.0.0.patch;
    cryptonite-openssl = pkgs.haskell.lib.appendPatch super.cryptonite-openssl ./cryptonite-openssl-0.7.patch;
    x509-system = pkgs.haskell.lib.appendPatch super.x509-system ./x509-system-1.6.6.patch;
    conduit = pkgs.haskell.lib.appendPatch super.conduit ./conduit-1.3.0.2.patch;
    rocksdb-haskell-ng = pkgs.haskell.lib.appendPatch super.rocksdb-haskell-ng ./rocksdb-haskell-ng.patch;
    file-embed-lzma = pkgs.haskell.lib.appendPatch super.file-embed-lzma ./file-embed-lzma-0.patch;
    
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
    cardano-sl-crypto     = doTemplateHaskellVerbose super.cardano-sl-crypto;
    cardano-sl-networking = doTemplateHaskell super.cardano-sl-networking;
    cardano-sl-core       = doTemplateHaskell super.cardano-sl-core;
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
    cardano-sl-client     = doTemplateHaskell super.cardano-sl-client;
    cardano-sl-generator  = doTemplateHaskell super.cardano-sl-generator;
    cardano-sl-wallet     = doTemplateHaskell super.cardano-sl-wallet;
    cardano-sl-wallet-new = doSymlinkLibs (doTemplateHaskell (addGitRev super.cardano-sl-wallet-new));

    trifecta              = doTemplateHaskell super.trifecta;
    cardano-sl-tools      = doTemplateHaskell (addGitRev super.cardano-sl-tools);
  };
} // { pkgs-x = pkgs; }
