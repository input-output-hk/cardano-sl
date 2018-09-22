let
  localLib = import ./lib.nix;
  jemallocOverlay = self: super: {
    # jemalloc has a bug that caused cardano-sl-db to fail to link (via
    # rocksdb, which can use jemalloc).
    # https://github.com/jemalloc/jemalloc/issues/937
    # Using jemalloc 510 with the --disable-initial-exec-tls flag seems to
    # fix it.
    jemalloc = self.callPackage ./nix/jemalloc/jemalloc510.nix {};
  };
in
{ system ? builtins.currentSystem
, config ? {}
, pkgs ? (import (localLib.fetchNixPkgs) { inherit system config; overlays = [ jemallocOverlay ]; })
, cardanoPkgs ? import ./. {inherit config system pkgs; }
}:
with pkgs;
let
  getCardanoSLDeps = with lib;
    ps: filter (drv: !(localLib.isCardanoSL drv.name))
    (concatMap haskell.lib.getHaskellBuildInputs
     (attrValues (filterAttrs isWantedDep ps)));
  isWantedDep = name: drv: localLib.isCardanoSL name && !(drv ? "gitrev");
  ghc = cardanoPkgs.ghc.withPackages getCardanoSLDeps;

  stackDeps = [
    zlib openssh autoreconfHook openssl
    gmp rocksdb git bsdiff ncurses lzma
    perl bash
  ];
  # TODO: add cabal-install (2.0.0.1 won't work)
  devTools = [ hlint cardanoPkgs.stylish-haskell ];

  cardanoSL = haskell.lib.buildStackProject {
    inherit ghc;
    name = "cardano-sl-env";

    buildInputs = devTools ++ stackDeps
      # cabal-install and stack pull in lots of dependencies on OSX so skip them
      # See https://github.com/NixOS/nixpkgs/issues/21200
      ++ (lib.optionals stdenv.isLinux [ stack ])
      ++ (lib.optionals stdenv.isDarwin (with darwin.apple_sdk.frameworks; [ Cocoa CoreServices libcxx libiconv ]));

    shellHook = lib.optionalString lib.inNixShell ''
      eval "$(egrep ^export ${ghc}/bin/ghc)"
      export PATH=${ghc}/bin:$PATH
    '';

    phases = ["nobuildPhase"];
    nobuildPhase = "mkdir -p $out";
  };

  fixStylishHaskell = stdenv.mkDerivation {
    name = "fix-stylish-haskell";
    buildInputs = [ cardanoPkgs.stylish-haskell git ];
    shellHook = ''
      git diff > pre-stylish.diff
      find . -type f -not -path '.git' -not -path '*.stack-work*' -name "*.hs" -not -name 'HLint.hs' -exec stylish-haskell -i {} \;
      git diff > post-stylish.diff
      diff pre-stylish.diff post-stylish.diff > /dev/null
      if [ $? != 0 ]
      then
        echo "Changes by stylish have been made. Please commit them."
      else
        echo "No stylish changes were made."
      fi
      rm pre-stylish.diff post-stylish.diff
      exit
    '';
  };
  updateEverythingCabal = stdenv.mkDerivation {
    name = "updateEverythingCabal";
    buildInputs = [ pkgs.cabal2nix ];
    shellHook = ''
      cp ${iohkPkgs.everythingCabal} everything.cabal
      cabal2nix ./. > everything.nix
      cabal2nix cabal://Cabal-2.2.0.0 > cabal-merger/cabal.nix
      exit
    '';
  };
in cardanoSL // {
  inherit fixStylishHaskell updateEverythingCabal;
}
