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
  cardanoPkgs = ((import ./pkgs { inherit pkgs; }).override {
    overrides = self: super: {
      cardano-sl = overrideCabal super.cardano-sl (drv: {
        patchPhase = ''
          export CSL_SYSTEM_TAG=${if pkgs.stdenv.isDarwin then "macos" else "linux64"}
        '';
        # production full nodes shouldn't use wallet as it means different constants
        configureFlags = [
          "-f-asserts"
          "-f-dev-mode"
          "--ghc-option=-optl-lm"
        ];
        testTarget = "--test-option='--skip=Test.Pos.Update.Poll.Poll.PollState.PurePoll' --log=test.log || (sleep 10 && kill $TAILPID && false)";
        preCheck = ''
          mkdir -p dist/test
          touch dist/test/test.log
          tail -F dist/test/test.log &
          export TAILPID=$!
        '';
        postCheck = ''
          sleep 10
          kill $TAILPID
        '';
        # waiting on load-command size fix in dyld
        doCheck = ! pkgs.stdenv.isDarwin;
      });
      cardano-sl-core = overrideCabal super.cardano-sl-core (drv: {
        configureFlags = [
          "-f-embed-config"
          "-f-asserts"
          "-f-dev-mode"
          "--ghc-options=-DCONFIG=${dconfig}"
          "--ghc-options=-DGITREV=${gitrev}"
        ];
      });
      cardano-sl-wallet = justStaticExecutables super.cardano-sl-wallet;
      cardano-sl-tools = justStaticExecutables (overrideCabal super.cardano-sl-tools (drv: {
        # waiting on load-command size fix in dyld
        doCheck = ! pkgs.stdenv.isDarwin;
      }));

      cardano-sl-static = justStaticExecutables self.cardano-sl;
      cardano-sl-explorer-static = justStaticExecutables self.cardano-sl-explorer;
      cardano-report-server-static = justStaticExecutables self.cardano-report-server;

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
  other = {
    stack2nix = import (pkgs.fetchFromGitHub {
      owner = "input-output-hk";
      repo = "stack2nix";
      rev = "be52e67113332280911bcc4924d42f90e21f1144";
      sha256 = "13n7gjyzll3prvdsb6kjyxk9g0by5bv0q34ld7a2nbvdcl1q67fb";
    }) { inherit pkgs; };
  };
in cardanoPkgs // other
