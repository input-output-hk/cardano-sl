{ system ? builtins.currentSystem
, config ? {}
, pkgs ? (import ((import ./../lib.nix).fetchNixPkgs) { inherit system config; }) }:

with pkgs;

stdenv.mkDerivation {
  name = "daedalus-bridge";

  buildInputs = [ nodejs nodePackages.bower purescript ];

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
