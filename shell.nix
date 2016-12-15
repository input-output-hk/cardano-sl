with import <nixpkgs> { };
let
  hsPkgs = haskell.packages.ghc801;
in
  haskell.lib.buildStackProject {
     name = "cardano-sl";
     ghc = hsPkgs.ghc;
     # cabal-install and stack pull in lots of dependencies on OSX so skip them
     buildInputs = [
       zlib openssh autoreconfHook openssl
       gmp rocksdb git ] ++ (lib.optionals stdenv.isLinux [ cabal-install stack ]);
     LANG = "en_US.UTF-8";
  }
