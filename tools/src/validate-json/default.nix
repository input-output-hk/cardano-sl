{ stdenv, python36 }:

stdenv.mkDerivation {
  name = "validate-json";
  buildInputs = [
    (python36.withPackages (pythonPackages: with pythonPackages; [
      jsonschema
      docopt
    ]))
  ];
  unpackPhase = ":";
  installPhase = "install -m755 -D ${./validate_json.py} $out/bin/validate_json";
}
