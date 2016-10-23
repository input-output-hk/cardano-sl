with import <nixpkgs> { };
haskell.lib.buildStackProject {
   ghc = haskell.packages.ghc801.ghc;
   name = "pos-prototype";
   buildInputs = [ zlib glib git cabal-install openssh autoreconfHook stack openssl];
}

