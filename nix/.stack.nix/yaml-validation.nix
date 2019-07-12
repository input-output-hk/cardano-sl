{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "yaml-validation"; version = "0.1"; };
      license = "NONE";
      copyright = "";
      maintainer = "";
      author = "";
      homepage = "";
      url = "";
      synopsis = "";
      description = "";
      buildType = "Simple";
      };
    components = {
      exes = {
        "yamlValidation" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.cardano-sl-util)
            (hsPkgs.cardano-sl-infra)
            (hsPkgs.split)
            (hsPkgs.yaml)
            (hsPkgs.exceptions)
            ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././yaml-validation; }