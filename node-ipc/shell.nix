{ commonLib ? import ../lib.nix
, iohkPkgs ? import ../. {}
, pkgs ? commonLib.pkgs
}:

pkgs.mkShell {
  name = "node-ipc-env";
  buildInputs = [ pkgs.nodejs iohkPkgs.nix-tools.exes.cardano-wallet ];
}
