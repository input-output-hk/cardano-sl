with import <nixpkgs> { };
let
  hsPkgs = haskell.packages.ghc801;
in
  haskell.lib.buildStackProject {
     name = "cardano-sl";
     ghc = hsPkgs.ghc;
     buildInputs =
       [ zlib openssh autoreconfHook openssl
         gmp rocksdb git ];
     LANG = "en_US.UTF-8";
  }
