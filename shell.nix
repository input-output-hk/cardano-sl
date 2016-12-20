with import (fetchTarball https://github.com/NixOS/nixpkgs/archive/e18dac705ad36482880e23d0a89c60c3514cb446.tar.gz) { };

let
  hsPkgs = haskell.packages.ghc801;
in
  haskell.lib.buildStackProject {
     name = "cardano-sl";
     ghc = hsPkgs.ghc;
     buildInputs = [
       zlib openssh autoreconfHook openssl
       gmp rocksdb git
     # cabal-install and stack pull in lots of dependencies on OSX so skip them
     ] ++ (lib.optionals stdenv.isLinux [ cabal-install stack ]);
     LANG = "en_US.UTF-8";
  }
