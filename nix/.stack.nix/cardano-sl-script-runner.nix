{ system
, compiler
, flags
, pkgs
, hsPkgs
, pkgconfPkgs
, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = {
        name = "cardano-sl-script-runner";
        version = "3.0.2";
      };
      license = "MIT";
      copyright = "2018 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Cardano SL - Script Runner";
      description = "Cardano SL - ScriptRunner";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.aeson)
          (hsPkgs.brick)
          (hsPkgs.bytestring)
          (hsPkgs.cardano-sl)
          (hsPkgs.cardano-sl-chain)
          (hsPkgs.cardano-sl-client)
          (hsPkgs.cardano-sl-core)
          (hsPkgs.cardano-sl-crypto)
          (hsPkgs.cardano-sl-db)
          (hsPkgs.cardano-sl-infra)
          (hsPkgs.cardano-sl-networking)
          (hsPkgs.cardano-sl-util)
          (hsPkgs.conduit)
          (hsPkgs.constraints)
          (hsPkgs.containers)
          (hsPkgs.data-default)
          (hsPkgs.dns)
          (hsPkgs.formatting)
          (hsPkgs.lens)
          (hsPkgs.lifted-async)
          (hsPkgs.mtl)
          (hsPkgs.optparse-applicative)
          (hsPkgs.process)
          (hsPkgs.resourcet)
          (hsPkgs.serokell-util)
          (hsPkgs.stm)
          (hsPkgs.text)
          (hsPkgs.time)
          (hsPkgs.time-units)
          (hsPkgs.turtle)
          (hsPkgs.universum)
          (hsPkgs.unix)
          (hsPkgs.unordered-containers)
          (hsPkgs.vector)
          (hsPkgs.vty)
          (hsPkgs.yaml)
        ];
      };
      exes = {
        "testcases" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.cardano-sl)
            (hsPkgs.cardano-sl-binary)
            (hsPkgs.cardano-sl-chain)
            (hsPkgs.cardano-sl-core)
            (hsPkgs.cardano-sl-db)
            (hsPkgs.cardano-sl-infra)
            (hsPkgs.cardano-sl-script-runner)
            (hsPkgs.cardano-sl-util)
            (hsPkgs.cborg)
            (hsPkgs.constraints)
            (hsPkgs.data-default)
            (hsPkgs.formatting)
            (hsPkgs.serokell-util)
            (hsPkgs.text)
            (hsPkgs.time-units)
            (hsPkgs.turtle)
            (hsPkgs.universum)
          ];
        };
      };
    };
  } // rec {
    src = .././../script-runner;
  }