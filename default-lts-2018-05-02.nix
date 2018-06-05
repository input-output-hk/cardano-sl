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
pkgs.haskellPackages.override {
  overrides = self: super: builtins.trace super {
    #fetch a package candidate from hackage and return the cabal2nix expression.
    hackageCandidate = name: ver: args: self.callCabal2nix name (fetchTarball "https://hackage.haskell.org/package/${name}-${ver}/candidate/${name}-${ver}.tar.gz") args;
    #libiserv = with haskell.lib; addExtraLibrary (enableCabalFlag (self.hackageCandidate "libiserv" "8.5" {}) "network") self.network;
    #iserv-proxy = self.hackageCandidate "iserv-proxy" "8.5" { libiserv = self.libiserv; };
    # TODO: Why is `network` not properly propagated from `libiserv`?
    remote-iserv = with pkgs.haskell.lib; let pkg = addExtraLibrary super.remote-iserv self.network; in
      overrideCabal (addBuildDepends pkg [ windows.mingw_w64_pthreads ]) (drv: {
        postInstall = ''
          cp ${windows.mingw_w64_pthreads}/bin/libwinpthread-1.dll $out/bin/
        '';
      });
  };
} // { pkgs-x = pkgs; }
