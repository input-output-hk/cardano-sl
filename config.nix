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
              configureFlags = (drv.configureFlags or []) ++ [ spaces ];
            });
          };
        });
      };
    };
  };
