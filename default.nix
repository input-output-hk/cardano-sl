let
  localLib = import ./lib.nix;
in
{ system ? builtins.currentSystem
, config ? {}
, dconfig ? "testnet_staging"
, gitrev ? "unknown"
, pkgs ? (import (localLib.fetchNixPkgs) { inherit system config; }) }:

with pkgs.lib;
with pkgs.haskell.lib;

let
  addConfigureFlags = flags: drv: overrideCabal drv (drv: {
    configureFlags = flags;
  });
  cleanSource2 = builtins.filterSource (name: type: let
      f1 = cleanSourceFilter name type;
      baseName = baseNameOf (toString name);
      f2 = ! (type == "symlink" && hasSuffix ".root" baseName);
      f3 = ! (baseName == ".stack-work");
      f4 = ! (hasSuffix ".nix" baseName);
      f5 = ! (hasSuffix ".swp" baseName);
    in f1 && f2 && f3 && f4 && f5);
in ((import ./pkgs { inherit pkgs; }).override {
  overrides = self: super: {
    cardano-sl = overrideCabal super.cardano-sl (drv: {
      src = cleanSource2 drv.src;
      patchPhase = ''
       export CSL_SYSTEM_TAG=${if pkgs.stdenv.isDarwin then "macos" else "linux64"}
      '';
      # production full nodes shouldn't use wallet as it means different constants
      configureFlags = [
        "-f-asserts"
        "-f-dev-mode"
        "--ghc-option=-optl-lm"
      ];
      # waiting on load-command size fix in dyld
      doCheck = ! pkgs.stdenv.isDarwin;
    });
    cardano-sl-core = overrideCabal super.cardano-sl-core (drv: {
      src = cleanSource2 drv.src;
      configureFlags = [
        "-f-embed-config"
        "-f-asserts"
        "-f-dev-mode"
        "--ghc-options=-DCONFIG=${dconfig}"
        "--ghc-options=-DGITREV=${gitrev}"
      ];
    });
    cardano-sl-tools = overrideCabal super.cardano-sl-tools (drv: {
      src = cleanSource2 drv.src;
      # waiting on load-command size fix in dyld
      doCheck = ! pkgs.stdenv.isDarwin;
    });
    # TODO: patch cabal2nix to allow this
    cardano-sl-db = overrideCabal super.cardano-sl-db (drv: { src = cleanSource2 drv.src; });
    cardano-sl-infra = overrideCabal super.cardano-sl-infra (drv: { src = cleanSource2 drv.src; });
    cardano-sl-lrc = overrideCabal super.cardano-sl-lrc (drv: { src = cleanSource2 drv.src; });
    cardano-sl-ssc = overrideCabal super.cardano-sl-ssc (drv: { src = cleanSource2 drv.src; });
    cardano-sl-txp = overrideCabal super.cardano-sl-txp (drv: { src = cleanSource2 drv.src; });
    cardano-sl-update = overrideCabal super.cardano-sl-update (drv: { src = cleanSource2 drv.src; });
    cardano-sl-godtossing = overrideCabal super.cardano-sl-godtossing (drv: { src = cleanSource2 drv.src; });
    cardano-sl-lwallet = overrideCabal super.cardano-sl-lwallet (drv: { src = cleanSource2 drv.src; });

    cardano-sl-static = justStaticExecutables self.cardano-sl;
    cardano-sl-explorer-static = justStaticExecutables self.cardano-sl-explorer;

    # Waiting on cryptonite release 0.25
    cryptonite = addConfigureFlags ["--ghc-option=-optl-pthread"] super.cryptonite;

    # Due to https://github.com/input-output-hk/stack2nix/issues/56
    hfsevents = self.callPackage ./pkgs/hfsevents.nix { inherit (pkgs.darwin.apple_sdk.frameworks) Cocoa CoreServices; };

    mkDerivation = args: super.mkDerivation (args // {
      #enableLibraryProfiling = true;
    });
  };
}) // {
  stack2nix = import (pkgs.fetchFromGitHub {
    owner = "input-output-hk";
    repo = "stack2nix";
    rev = "be52e67113332280911bcc4924d42f90e21f1144";
    sha256 = "13n7gjyzll3prvdsb6kjyxk9g0by5bv0q34ld7a2nbvdcl1q67fb";
  }) { inherit pkgs; };
}
