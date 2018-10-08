{
    packageOverrides = ps: with ps; rec {
      haskell = lib.recursiveUpdate ps.haskell {

        # GHC 8.2.2 Customizations
        compiler.ghc822 = (ps.haskell.compiler.ghc822.override {
        }).overrideAttrs (drv: {
           patches = (drv.patches or []) ++ [
             ./ghc-8.0.2-darwin-rec-link.patch
           ];
           # postPatch = (drv.postPath or "") + ''
           # autoreconf
           # '';
        });
        packages.ghc822 = (ps.haskell.packages.ghc822.override {
          overrides = self: super: rec {};
        });
        # # GHC 8.4.3 Customizations 
        # compiler.ghc843 = (ps.haskell.compiler.ghc843.override {
        #    ghcFlavour = if ps.stdenv.targetPlatform == ps.stdenv.hostPlatform
        #                 then "perf"
        #                 else "perf-cross-ncg";
        #    enableShared = ps.stdenv.targetPlatform == ps.stdenv.hostPlatform;
        #    enableIntegerSimple = false;
        #  }).overrideAttrs (drv: {
        #    dontStrip = true;
        #    hardeningDisable = [ "stackprotector" "format" ];
        #    patches = (drv.patches or []) ++ [
        #      ./move-iserv-8.4.2.patch
        #      ./hsc2hs-8.4.2.patch
        #      ./various-8.4.2.patch
        #      ./lowercase-8.4.2.patch
        #      ./cabal-exe-ext-8.4.2.patch
        #      ./dll-loader-8.4.2.patch
        #      ./outputtable-assert-8.4.2.patch
        #      ./0001-Stop-the-linker-panic.patch
        #      ./ghc-8.4.3-Cabal2201-SMP-test-fix.patch
        #      ./ghc-8.4.3-Cabal2201-no-hackage-tests.patch
        #      ./ghc-8.4.3-Cabal2201-allow-test-wrapper.patch
        #      ./ghc-8.4.3-Cabal2201-response-file-support.patch
        #    ];
        #    postPatch = (drv.postPath or "") + ''
        #    autoreconf
        #    '';
        #  });
      };
   };
}
