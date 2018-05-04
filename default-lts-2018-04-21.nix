let

  stack-pkgs = import ./stack-pkgs.nix;

  overlay = self: super: {
    haskellPackages = (import <stackage>).lts-2018-04-21
      { extraDeps = hsPkgs: (stack-pkgs.extraDeps hsPkgs
                          // stack-pkgs.packages hsPkgs); };
  };

  pkgs = import <nixpkgs> { overlays = [ overlay ]; };

in with pkgs.haskellPackages;
# pkgs.lib.mapAttrs (_: x: callPackage x {})
pkgs.haskellPackages.override {
  overrides = self: super: {
    # FIXME: this doesn't work yet. Overridable logic
    #        is missing.
  };
}
