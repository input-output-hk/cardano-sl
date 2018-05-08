{ localLib ? import ./../../../lib.nix
}:

with localLib;

let
  executable = {
    gen-swagger-file = "${iohkPkgs.cardano-sl-tools}/bin/cardano-swagger-file";
    validation-json = import ./../../../tools/src/validate-json;
  };
  schema = ./../../../tools/src/validate-json/swagger-meta-2.0.json;
in pkgs.writeScript "validate-swagger-schema" ''
  ${gen-swagger-file} --target wallet@v0 --output-file swagger.v0.json
  ${validate-json} --schema ${schema} swagger.v0.json
  ${gen-swagger-file} --target wallet@v1 --output-file swagger.v1.json
  ${validate-json} --schema ${schema} swagger.v1.json
  rm swagger.*.json
''
