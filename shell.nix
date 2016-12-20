with import (fetchTarball https://github.com/NixOS/nixpkgs/archive/5ed1aee3af6ce80c63e534fc26d7ee611d4ca50d.tar.gz) { };

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
