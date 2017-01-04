with import (fetchTarball https://github.com/NixOS/nixpkgs/archive/8b451398ef7d0a5f4ecd1f5e2593fa1348e80727.tar.gz) { };

let
  hsPkgs = haskell.packages.ghc802;
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
