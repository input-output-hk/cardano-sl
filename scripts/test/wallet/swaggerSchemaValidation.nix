{ localLib ? import ./../../../lib.nix
, pkgs ? (import (localLib.fetchNixPkgs) { inherit system config; })
, system ? builtins.currentSystem
, config ? {}
, gitrev ? "123456"
}:

with localLib;

let
  iohkPkgs = import ./../../.. { inherit config system pkgs gitrev; };
  generate-swagger-file = "${iohkPkgs.cardano-sl-wallet-new}/bin/cardano-generate-swagger-file";
  validate-json = "${iohkPkgs.validateJson}/bin/validate_json";
  schema = ./../../../tools/src/validate-json/swagger-meta-2.0.json;
in pkgs.writeScript "validate-swagger-schema" ''
  ${generate-swagger-file} --target wallet@v0 --output-file swagger.v0.json
  ${generate-swagger-file} --target wallet@v1 --output-file swagger.v1.json
  ${validate-json} --schema ${schema} swagger.v0.json && ${validate-json} --schema ${schema} swagger.v1.json
  EXIT_STATUS=$?
  rm -f swagger.*.json
  exit $EXIT_STATUS
''
