with import <nixpkgs> { };
let
  hsPkgs = haskell.packages.ghc801;
in
  haskell.lib.buildStackProject {
     name = "cardano-sl";
     ghc = hsPkgs.ghc;
     buildInputs =
       [ zlib glib git cabal-install openssh autoreconfHook stack openssl
         sshpass gmp ];
     LANG = "en_US.UTF-8";
  }
