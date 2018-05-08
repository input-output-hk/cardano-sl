with import <nixpkgs> {};

{
  validateJson = python35.pkgs.buildPythonPackage rec {
    name = "validateJson";
    src = ./.;
    propagatedBuildInputs = with python35.pkgs; [jsonschema docopt];
  };
}
