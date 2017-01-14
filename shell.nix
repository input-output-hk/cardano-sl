with import (fetchTarball https://github.com/NixOS/nixpkgs/archive/663048f37849667f84bb1d941132369eff6e630f.tar.gz) { };

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
     # See https://github.com/NixOS/nixpkgs/issues/21200
     ] ++ (lib.optionals stdenv.isLinux [ cabal-install stack ]);
     LANG = "en_US.UTF-8";
     # Needed to be changed to clever conditional
     CSL_SYSTEM_TAG = "linux64";
  }
