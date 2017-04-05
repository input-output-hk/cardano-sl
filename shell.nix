with import (fetchTarball https://github.com/NixOS/nixpkgs/archive/763e21e982370f67c126f92a1113ea949db3b6e0.tar.gz) { };

let
  hsPkgs = haskell.packages.ghc802;
in
  haskell.lib.buildStackProject {
     name = "cardano-sl-explorer";
     ghc = hsPkgs.ghc;
     buildInputs = [
       zlib openssh autoreconfHook openssl
       gmp rocksdb git bsdiff
     # cabal-install and stack pull in lots of dependencies on OSX so skip them
     # See https://github.com/NixOS/nixpkgs/issues/21200
     ] ++ (lib.optionals stdenv.isLinux [ cabal-install stack ]);
     LANG = "en_US.UTF-8";
  }
