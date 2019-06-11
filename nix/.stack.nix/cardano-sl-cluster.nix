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
        name = "cardano-sl-cluster";
        version = "3.0.2";
      };
      license = "MIT";
      copyright = "2018 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK Engineering Team";
      homepage = "https://github.com/input-output-hk/cardano-sl/cluster/README.md";
      url = "";
      synopsis = "Utilities to generate and run cluster of nodes";
      description = "See README";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.cardano-sl)
          (hsPkgs.cardano-sl-chain)
          (hsPkgs.cardano-sl-core)
          (hsPkgs.cardano-sl-infra)
          (hsPkgs.cardano-sl-networking)
          (hsPkgs.cardano-sl-node)
          (hsPkgs.cardano-sl-util)
          (hsPkgs.cardano-sl-x509)
          (hsPkgs.aeson)
          (hsPkgs.async)
          (hsPkgs.attoparsec)
          (hsPkgs.bytestring)
          (hsPkgs.containers)
          (hsPkgs.directory)
          (hsPkgs.filepath)
          (hsPkgs.formatting)
          (hsPkgs.iproute)
          (hsPkgs.lens)
          (hsPkgs.optparse-applicative)
          (hsPkgs.parsec)
          (hsPkgs.safe)
          (hsPkgs.servant-client)
          (hsPkgs.temporary)
          (hsPkgs.text)
          (hsPkgs.time)
          (hsPkgs.tls)
          (hsPkgs.universum)
        ];
      };
      exes = {
        "cardano-sl-cluster-demo" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.cardano-sl)
            (hsPkgs.cardano-sl-cluster)
            (hsPkgs.cardano-sl-node)
            (hsPkgs.ansi-terminal)
            (hsPkgs.async)
            (hsPkgs.containers)
            (hsPkgs.docopt)
            (hsPkgs.formatting)
            (hsPkgs.lens)
            (hsPkgs.universum)
          ];
        };
        "cardano-sl-cluster-prepare-environment" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.cardano-sl-cluster)
            (hsPkgs.containers)
            (hsPkgs.docopt)
            (hsPkgs.formatting)
            (hsPkgs.lens)
            (hsPkgs.universum)
          ];
        };
      };
      tests = {
        "cardano-sl-cluster-test" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.cardano-sl-cluster)
            (hsPkgs.cardano-sl-core)
            (hsPkgs.cardano-sl-infra)
            (hsPkgs.async)
            (hsPkgs.containers)
            (hsPkgs.lens)
            (hsPkgs.QuickCheck)
            (hsPkgs.time)
            (hsPkgs.universum)
          ];
        };
      };
    };
  } // rec {
    src = .././../cluster;
  }