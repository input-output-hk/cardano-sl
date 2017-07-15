with (import ((import ../lib.nix).fetchNixPkgs) {});

stdenv.mkDerivation {
  name = "daedalus-bridge";

  buildInputs = [ nodejs-7_x nodePackages.bower purescript ];

  src = null;
}
