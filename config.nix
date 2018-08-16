spaces:
{
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
          ghcFlavour = if ps.stdenv.targetPlatform == ps.stdenv.hostPlatform
                       then "perf"
                       else "perf-cross-ncg";
          enableShared = ps.stdenv.targetPlatform == ps.stdenv.hostPlatform;
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
            ./0001-Stop-the-linker-panic.patch
            ./ghc-8.4.3-Cabal2201-SMP-test-fix.patch
            ./ghc-8.4.3-Cabal2201-no-hackage-tests.patch
            ./ghc-8.4.3-Cabal2201-allow-test-wrapper.patch
            ./ghc-8.4.3-Cabal2201-response-file-support.patch
          ];
          postPatch = (drv.postPath or "") + ''
          autoreconf
          '';
        });
        packages.ghc843 = ps.haskell.packages.ghc843.override {
          overrides = self: super: rec {
            mkDerivation = drv: super.mkDerivation (drv // {
              # # fast builds -- the logic is as follows:
              # #  - test are often broken and we have a curated set
              # #    thus, let us assume we don't need no tests. (also time consuming)
              # #  - haddocks are not used, and sometimes fail.  (also time consuming)
              # #  - The curated set has proper version bounds, so we can just
              # #    exactConfig globally
              enableLibraryProfiling = false;
              enableSharedLibraries = ps.stdenv.buildPlatform == ps.stdenv.hostPlatform;
              enableSharedExecutables = false;
              # enableExecutableProfiling = false;
              doHaddock = false;
              doHoogle = false;
              doCheck = true;
              configureFlags = (drv.configureFlags or []) ++ [ spaces ];
            } // lib.optionalAttrs ps.stdenv.hostPlatform.isWindows (
              let wineTestWrapper = ps.writeScriptBin "test-wrapper" ''
                #!${ps.stdenv.shell}
                set -euo pipefail
                WINEDLLOVERRIDES="winemac.drv=d" WINEDEBUG=-all+error LC_ALL=en_US.UTF-8 WINEPREFIX=$TMP ${pkgs.buildPackages.winePackages.minimal}/bin/wine64 $@*
              ''; in {
              preCheck = ''
              echo "================================================================================"
              echo "RUNNING TESTS for ${drv.pname} via wine64"
              echo "================================================================================"
              # copy all .dlls into the local directory.
              # we ask ghc-pkg for *all* dynamic-library-dirs and then iterate over the unique set
              # to copy over dlls as needed.
              for libdir in $(ghc-pkg --package-db=$packageConfDir field "*" dynamic-library-dirs --simple-output|xargs|sed 's/ /\n/g'|sort -u); do
                if [ -d "$libdir" ]; then
                  for lib in "$libdir"/*.{DLL,dll}; do
                    cp "$lib" .
                  done
                fi
              done
              '';
              postCheck = ''
              echo "================================================================================"
              echo "END RUNNING TESTS for ${drv.pname}"
              echo "================================================================================"
              '';
              testTarget = "--test-wrapper ${wineTestWrapper}/bin/test-wrapper";
            })
          );
        };
      };
    };
  };
}
