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
  packageOverlay = self: super: {
    # the lts-XX_Y is essentially the same as what pkgs/default.nix returns.
    haskellPackages = ((import <stackage> { pkgs = super; }).lts-12_0
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
, target ? null
,n ? 0 }:
let
  # this is some hack to append spaces to the configureFlags of the
  # default derivation in GHC.  This allows us to force a rebuild
  # of all haskell packages.
  spaces = let repeat = n: c: c + (if n == 0 then "" else repeat (n - 1) c); in repeat n " ";
  
  # configure out general nix setup.
  # 
  config = 
  # Combine the Overlay, and Config to produce
  # our package set.
  pkgs = import <nixpkgs> ({
    inherit system;
    overlays = [ jmallocOverlay packageOverlay ];
    config = config;
  } // (if target != null
        then { crossSystem = { win64   = (import <nixpkgs/lib>).systems.examples.mingwW64;
                               macOS   = abort "macOS target not available";
                               rpi     = abort "Raspberry Pi target not available";
                               ios     = abort "iOS target not available";
                               android = abort "Android target not available"; }."${target}"; }
        else {}));

in with pkgs.haskellPackages;
let ps = (with pkgs.haskell.lib; pkgs.haskellPackages.override rec {
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
      # unset the configureFlags.
      # configure should have run already
      # without restting it, wine might fail
      # due to a too large environment.
      unset configureFlags
      PORT=$((5000 + $RANDOM % 5000))
      echo "---> Starting remote-iserv on port $PORT"
      WINEPREFIX=$TMP wine64 ${self.remote-iserv}/bin/remote-iserv.exe tmp $PORT &
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

  appendPatchMingw = pkg: p:
    with pkg.stdenv;
      if hostPlatform.isWindows
      then appendPatch pkg p
      else pkg;

  addGitRev = subject: subject.overrideAttrs (drv: { GITREV = "blahblahblah"; });
  doTemplateHaskellVerbose = pkg: with pkgs.haskell.lib; let
    buildTools = [ buildHaskellPackages.iserv-proxy pkgs.buildPackages.winePackages.minimal ];
    buildFlags = map (opt: "--ghc-option=" + opt) [
      "-fexternal-interpreter"
      "-pgmi" "${buildHaskellPackages.iserv-proxy}/bin/iserv-proxy"
      "-opti" "127.0.0.1" "-opti" "$PORT" # "-opti" "-v" #"-xc" "+RTS" "-Di"
      # TODO: this should be automatically injected based on the extraLibrary.
      "-L${pkgs.windows.mingw_w64_pthreads}/lib"
    ];
    preBuild = ''
      PORT=$((5000 + $RANDOM % 5000))
      echo "" | xargs --show-limits echo
      unset configureFlags
      echo "" | xargs --show-limits echo
      echo "---> Starting remote-iserv on port $PORT"
      WINEPREFIX=$TMP wine64 ${self.remote-iserv}/bin/remote-iserv.exe tmp $PORT & # -v +RTS -Di &
      echo "---| called wine ..."
      for i in {1..5}; do
        sleep 1 # wait for wine to fully boot up...
        echo -n "."
      done
      echo ""
      echo "---| remote-iserv should have started on $PORT"
      RISERV_PID=$!
    '';
    postBuild = ''
      echo "---> killing remote-iserv..."
      kill $RISERV_PID
      echo "Sleeping another 600s..."
#      sleep 600
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
    streaming-commons     = appendPatchMingw super.streaming-commons  ./streaming-commons-0.2.0.0.patch;
    cryptonite-openssl    = appendPatchMingw super.cryptonite-openssl ./cryptonite-openssl-0.7.patch;
    x509-system           = appendPatchMingw super.x509-system        ./x509-system-1.6.6.patch;
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
  };
} // { pkgs-x = pkgs; });
in ps // {
    # From the appveyor.yaml script
    # # We intentionally don't build auxx here, because this build is for installer.
    #  - scripts\ci\appveyor-retry call stack --dump-logs install cardano-sl cardano-sl-tools cardano-sl-wallet cardano-sl-wallet-new
    #      -j 3
    #      --no-terminal
    #      --local-bin-path %WORK_DIR%
    #      --no-haddock-deps
    #      --flag cardano-sl-core:-asserts
    #      --flag cardano-sl-tools:for-installer
    #      --flag cardano-sl-wallet:for-installer
    #      --extra-include-dirs="C:\OpenSSL-Win64-v102\include"
    #      --extra-lib-dirs="C:\OpenSSL-Win64-v102"
    #      --extra-include-dirs="C:\xz_extracted\include"
    #      --extra-lib-dirs="C:\xz_extracted\bin_x86-64"
    #      --extra-include-dirs="%WORK_DIR%\rocksdb\include"
    #      --extra-lib-dirs="%WORK_DIR%"
    #  # Cardano pieces, modulo the frontend
    #  - mkdir daedalus
    #    # log config is called `log-config-prod.yaml` just in case, it's the old name
    #  - copy log-configs\daedalus.yaml daedalus\log-config-prod.yaml
    #  - copy lib\configuration.yaml daedalus\
    #  - copy lib\*genesis*.json daedalus\
    #  - copy cardano-launcher.exe daedalus\
    #  - copy cardano-node.exe daedalus\
    #  - copy cardano-x509-certificates.exe daedalus\
    #  - cd daedalus
    #  - Echo %APPVEYOR_BUILD_VERSION% > build-id
    #  - Echo %APPVEYOR_REPO_COMMIT% > commit-id
    #  - Echo https://ci.appveyor.com/project/%APPVEYOR_ACCOUNT_NAME%/%APPVEYOR_PROJECT_SLUG%/build/%APPVEYOR_BUILD_VERSION% > ci-url
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
      echo "BAD_BUILD_VERSION" >                               daedalus/build-id
      echo "BAD_REPO_COMMIT"   >                               daedalus/commit-id
      echo "BAD_CI_URL"        >                               daedalus/ci-url
      cd daedalus
      zip $out/CardanoSL.zip *
      cd ..
      rm -fR daedalus
    '';
}
