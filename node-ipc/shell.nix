{ iohkPkgs ? import ../. { }
, pkgs ? iohkPkgs.pkgs
}:

pkgs.mkShell {
  name = "node-ipc-env";
  buildInputs = [ pkgs.nodejs iohkPkgs.cardanoPackages.cardano-wallet ];
}
