{
  # allow building for windows
  allowUnsupportedSystem = true;
  # we want the 64bit wine version
  wine.build = "wine64";

  # sadly we need to patch GHC a bit.
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
       rocksdb = with ps.stdenv;
         if hostPlatform.isWindows
         then pkgs.callPackage ./rocksdb-prebuilt.nix { inherit (buildPackages) fetchurl unzip; }
         else ps.rocksdb;

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
      compiler.ghc844 = (ps.haskell.compiler.ghc844.override {
        ghcFlavour = if ps.stdenv.targetPlatform == ps.stdenv.hostPlatform
                     then "perf"
                     else "perf-cross-ncg";
        enableShared = ps.stdenv.targetPlatform == ps.stdenv.hostPlatform;
        enableIntegerSimple = false;
      }).overrideAttrs (drv: {
        dontStrip = true;
        hardeningDisable = [ "stackprotector" "format" ];
        patches = (drv.patches or []) ++ [
          ./patches/ghc/move-iserv-8.4.2.patch
          ./patches/ghc/hsc2hs-8.4.2.patch
          ./patches/ghc/various-8.4.2.patch
          ./patches/ghc/lowercase-8.4.2.patch
          ./patches/ghc/cabal-exe-ext-8.4.2.patch
          ./patches/ghc/dll-loader-8.4.2.patch
          ./patches/ghc/outputtable-assert-8.4.2.patch
          ./patches/ghc/0001-Stop-the-linker-panic.patch
          ./patches/ghc/ghc-8.4.3-Cabal2201-SMP-test-fix.patch
          ./patches/ghc/ghc-8.4.3-Cabal2201-no-hackage-tests.patch
          ./patches/ghc/ghc-8.4.3-Cabal2201-allow-test-wrapper.patch
          ./patches/ghc/ghc-8.4.3-Cabal2201-response-file-support.patch
#          ./patches/ghc/ghc-8.4.4-darwin-rec-link.patch
        ];
        postPatch = (drv.postPath or "") + ''
        autoreconf
        '';
      });
    };
  };
}
