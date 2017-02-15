with (import (fetchTarball https://github.com/NixOS/nixpkgs/archive/d4787680bcc9c5163eec15756e871044b2220b4e.tar.gz) {});

stdenv.mkDerivation {
  name = "daedalus-bridge";

  buildInputs = [ nodejs-7_x nodePackages.bower haskellPackages.purescript ];

  src = null;
}
