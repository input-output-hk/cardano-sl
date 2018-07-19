{ pkgs }:
let nsis = pkgs.callPackage ./nsis.nix {};
in pkgs.runCommand "demo" { buildInputs = [ nsis ]; } ''
      mkdir $out
      cd $out
      cp -vi ${./example1.nsi} example1.nsi
      ${nsis}/bin/makensis example1.nsi
      ln -sv ${nsis} nsis
    ''
