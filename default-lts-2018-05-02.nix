let

  stack-pkgs = import ./stack-pkgs.nix;

  overlay = self: super: {
    haskellPackages = (import <stackage> { pkgs = super; }).lts-2018-05-02
      { extraDeps = hsPkgs: (stack-pkgs.extraDeps hsPkgs
                          // stack-pkgs.packages hsPkgs); };
  };

  config = {
    allowUnsupportedSystem = true;

    wine.build = "wine64";

    packageOverrides = ps: with ps; rec {
      haskell = lib.recursiveUpdate ps.haskell {
        compiler.ghc842 = (ps.haskell.compiler.ghc842.override {
          ghcCrossFlavour = "quick-cross-ncg";
          ghcFlavour = "quick";
          enableShared = false;
        }).overrideAttrs (drv: {
          dontStrip = true;
          hardeningDisable = [ "stackprotector" ];
          patches = (drv.patches or []) ++ [ ./move-iserv-8.4.2.patch ];
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
pkgs.haskellPackages.override {
  overrides = self: super: {
    # FIXME: this doesn't work yet. Overridable logic
    #        for packages is missing I believe.
  };
}
