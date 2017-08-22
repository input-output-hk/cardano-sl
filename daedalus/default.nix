with (import ((import ../lib.nix).fetchNixPkgs) {});

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
