{ pkgs, localLib, enableProfiling, gitrev, ghc }:

with pkgs.haskell.lib;
with localLib;

let
  justStaticExecutablesGitRev = import ../../scripts/set-git-rev {
    inherit pkgs gitrev ghc;
  };
  addRealTimeTestLogs = drv: overrideCabal drv (attrs: {
    testTarget = "--show-details=streaming";
  });
in

self: super: {

    ########################################################################
    # Overides of Cardano SL packages

    cardano-sl-core = overrideCabal super.cardano-sl-core (drv: {
      configureFlags = (drv.configureFlags or []) ++ [
        "-f-asserts"
      ];
    });
    cardano-sl = overrideCabal super.cardano-sl (drv: {
      # production full nodes shouldn't use wallet as it means different constants
      configureFlags = (drv.configureFlags or []) ++ [
        "-f-asserts"
      ];
      passthru = {
        inherit enableProfiling;
      };
    });
    cardano-sl-wallet-static = justStaticExecutablesGitRev super.cardano-sl-wallet;
    cardano-sl-client = addRealTimeTestLogs super.cardano-sl-client;
    cardano-sl-generator = addRealTimeTestLogs super.cardano-sl-generator;
    cardano-sl-networking = addRealTimeTestLogs super.cardano-sl-networking;
    cardano-sl-auxx-static = justStaticExecutablesGitRev super.cardano-sl-auxx;
    cardano-sl-wallet-new-static = justStaticExecutablesGitRev super.cardano-sl-wallet-new;
    cardano-sl-node-static = justStaticExecutablesGitRev self.cardano-sl-node;
    cardano-sl-explorer-static = justStaticExecutablesGitRev self.cardano-sl-explorer;
    cardano-report-server-static = justStaticExecutablesGitRev self.cardano-report-server;
    cardano-sl-faucet-static = justStaticExecutablesGitRev self.cardano-sl-faucet;
    cardano-sl-tools-static = justStaticExecutablesGitRev super.cardano-sl-tools;

    ########################################################################
    # The base Haskell package builder

    mkDerivation = args: super.mkDerivation (args // {
      enableLibraryProfiling = enableProfiling;
      enableExecutableProfiling = enableProfiling;
      # Static linking for everything to work around
      # https://ghc.haskell.org/trac/ghc/ticket/14444
      # This will be the default in nixpkgs since
      # https://github.com/NixOS/nixpkgs/issues/29011
      enableSharedExecutables = false;
    } // optionalAttrs (args ? src) {
      src = localLib.cleanSourceTree args.src;
    });

    ########################################################################
    # Configuration of overrides for other dependencies

    # Undo configuration-nix.nix change to hardcode security binary on darwin
    # This is needed for macOS binary not to fail during update system (using http-client-tls)
    # Instead, now the binary is just looked up in $PATH as it should be installed on any macOS
    x509-system = overrideDerivation super.x509-system (drv: {
      postPatch = ":";
    });

    # TODO: get rid of pthreads option once cryptonite 0.25 is released
    # DEVOPS-393: https://github.com/haskell-crypto/cryptonite/issues/193
    cryptonite = appendPatch (appendConfigureFlag super.cryptonite "--ghc-option=-optl-pthread") ../../pkgs/cryptonite-segfault-blake.patch;

    # Due to https://github.com/input-output-hk/stack2nix/issues/56
    hfsevents = self.callPackage ../../pkgs/hfsevents.nix { inherit (pkgs.darwin.apple_sdk.frameworks) Cocoa CoreServices; };
  }
