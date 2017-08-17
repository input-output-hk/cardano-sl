let
  localLib = import ./lib.nix;
in
{ system ? builtins.currentSystem
, config ? {}
, dconfig ? "testnet_staging"
, genesis ? null
, pkgs ? (import (localLib.fetchNixPkgs) { inherit system config; }) }:

with pkgs.lib;
with (import <nixpkgs/pkgs/development/haskell-modules/lib.nix> { inherit pkgs; });

let
  addConfigureFlags = flags: drv: overrideCabal drv (drv: {
    configureFlags = flags;
  });
  cleanSource2 = builtins.filterSource (name: type: let
      f1 = cleanSourceFilter name type;
      baseName = baseNameOf (toString name);
      f2 = ! (type == "symlink" && hasSuffix ".root" baseName);
      f3 = ! (hasSuffix ".swp" baseName);
      f4 = ! (baseName == ".stack-work");
      f5 = ! (hasSuffix ".nix" baseName);
      f6 = ! (hasSuffix ".root" baseName);
      f7 = if genesis == null then true else ! (baseName == "genesis-godtossing-qa.bin");
    in f1 && f2 && f3 && f4 && f5 && f6 && f7);
  cardanoPkgs = ((import ./pkgs { inherit pkgs; }).override {
    overrides = self: super: {
      cardano-sl = overrideCabal super.cardano-sl (drv: {
        src = cleanSource2 drv.src;
        testTarget = "--test-option='--skip=Block.Logic.Var'";
        doHaddock = false;
        patchPhase = ''
          export CSL_SYSTEM_TAG=${if pkgs.stdenv.isDarwin then "macos" else "linux64"}
        '';
        # production full nodes shouldn't use wallet as it means different constants
        configureFlags = [
          "-f-asserts"
          "-f-dev-mode"
          # https://github.com/NixOS/nixpkgs/pull/24692#issuecomment-306509337
          "--ghc-option=-optl-lm"
        ];
        doCoverage = false;
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
        ];
      } // optionalAttrs (genesis != null) {
        postUnpack = ''
          echo dir is
          pwd
          echo root $sourceRoot
          ls -ltrh $sourceRoot
          rm -v $sourceRoot/genesis*bin
          cp -vi ${genesis}/genesis-core.bin $sourceRoot/genesis-core-tns.bin
        '';
      });
      cardano-sl-godtossing = overrideCabal super.cardano-sl-godtossing (drv: {
        src = cleanSource2 drv.src;
      } // optionalAttrs (genesis != null) {
        postUnpack = ''
          echo dir is
          pwd
          echo root $sourceRoot
          echo todo
          ls -ltrh $sourceRoot
          rm -v $sourceRoot/genesis*.bin
          cp -vi ${genesis}/genesis-godtossing.bin $sourceRoot/genesis-godtossing-tns.bin
        '';
      });
      cardano-sl-tools = overrideCabal super.cardano-sl-tools (drv: {
        src = cleanSource2 drv.src;
        # waiting on load-command size fix in dyld
        doCheck = ! pkgs.stdenv.isDarwin;
      });
      cardano-sl-explorer-static = justStaticExecutables self.cardano-sl-explorer;

      # TODO: patch cabal2nix to allow this
      cardano-sl-db = overrideCabal super.cardano-sl-db (drv: { src = cleanSource2 drv.src; });
      cardano-sl-infra = overrideCabal super.cardano-sl-infra (drv: { src = cleanSource2 drv.src; });
      cardano-sl-lrc = overrideCabal super.cardano-sl-lrc (drv: { src = cleanSource2 drv.src; });
      cardano-sl-ssc = overrideCabal super.cardano-sl-ssc (drv: { src = cleanSource2 drv.src; });
      cardano-sl-txp = overrideCabal super.cardano-sl-txp (drv: { src = cleanSource2 drv.src; });
      cardano-sl-update = overrideCabal super.cardano-sl-update (drv: { src = cleanSource2 drv.src; });
      cardano-sl-lwallet = overrideCabal super.cardano-sl-lwallet (drv: { src = cleanSource2 drv.src; });

      cardano-sl-static = justStaticExecutables self.cardano-sl;
      cardano-without-explorer = disableCabalFlag self.cardano-sl "with-explorer";
      cardano-with-explorer = disableCabalFlag self.cardano-sl "with-explorer";

      # Gold linker fixes
      cryptonite = addConfigureFlags ["--ghc-option=-optl-pthread"] super.cryptonite;
      # Darwin fixes upstreamed in nixpkgs commit 71bebd52547f4486816fd320bb3dc6314f139e67
      hinotify = if pkgs.stdenv.isDarwin then self.hfsevents else super.hinotify;
      hfsevents = self.callPackage ./pkgs/hfsevents.nix { inherit (pkgs.darwin.apple_sdk.frameworks) Cocoa CoreServices; };
      fsnotify = if pkgs.stdenv.isDarwin
        then addBuildDepend (dontCheck super.fsnotify) pkgs.darwin.apple_sdk.frameworks.Cocoa
        else dontCheck super.fsnotify;
      mkDerivation = args: super.mkDerivation (args // {
        #enableLibraryProfiling = true;
      });
    };
  });
  other = rec {
    stack2nix = import (pkgs.fetchFromGitHub {
      owner = "input-output-hk";
      repo = "stack2nix";
      rev = "9e9676b919cc38df203fbfc1316891815e27c37b";
      sha256 = "0rsfwxrhrq72y2rai4sidpihlnxfjvnaaa7qk94179ghjqs47hvv";
    }) { inherit pkgs; };
    make-genesis = pkgs.callPackage ./tests/make-genesis.nix { inherit (cardanoPkgs) cardano-sl-tools; };
    testjob = (import ./default.nix { inherit pkgs; genesis = make-genesis; }).cardano-sl-static;
  };
in cardanoPkgs // other
