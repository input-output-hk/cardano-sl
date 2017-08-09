with (import (fetchTarball https://github.com/NixOS/nixpkgs/archive/8bed8fb53227932886ab23e5f5f9eabe139f8e9f.tar.gz) {});
# TODO: upgrade to purescript 0.11.5
#with (import ((import ../lib.nix).fetchNixPkgs) {});

stdenv.mkDerivation {
  name = "daedalus-bridge";

  buildInputs = [ nodejs-7_x nodePackages.bower purescript ];

  src = ./.;

  buildPhase = ''
    for x in node_modules/{rimraf,cross-env,webpack,purescript-psa}; do
      patchShebangs $x
    done
    export HOME=$NIX_BUILD_TOP/home
    npm run build:prod
  '';
  installPhase = ''
    mkdir $out
    cp -vir dist $out
  '';
}
