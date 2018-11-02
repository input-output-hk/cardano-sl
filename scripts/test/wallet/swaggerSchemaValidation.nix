{ writeScript, cardano-sl-wallet-new, validateJson }:

let
  generate-swagger-file = "${cardano-sl-wallet-new}/bin/cardano-generate-swagger-file";
  validate-json = "${validateJson}/bin/validate_json";
  schema = ./../../../tools/src/validate-json/swagger-meta-2.0.json;
in writeScript "validate-swagger-schema" ''
  ${generate-swagger-file} --target wallet@v1 --output-file swagger.v1.json
  ${validate-json} --schema ${schema} swagger.v1.json
  EXIT_STATUS=$?
  rm -f swagger.*.json
  exit $EXIT_STATUS
''
