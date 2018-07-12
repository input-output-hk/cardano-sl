{ compiler, flags ? {}, hsPkgs, pkgconfPkgs, pkgs, system }:
let
    _flags = {} // flags;
    in {
      flags = _flags;
      package = {
        specVersion = "1.10";
        identifier = {
          name = "cardano-sl-node";
          version = "1.3.0";
        };
        license = "MIT";
        copyright = "2016 IOHK";
        maintainer = "Serokell <hi@serokell.io>";
        author = "Serokell";
        homepage = "";
        url = "";
        synopsis = "Cardano SL simple node executable";
        description = "Please see README.md";
        buildType = "Simple";
      };
      components = {
        exes = {
          "cardano-node-simple" = {
            depends  = [
              hsPkgs.base
              hsPkgs.cardano-sl-crypto
              hsPkgs.cardano-sl-ssc
              hsPkgs.cardano-sl-infra
              hsPkgs.cardano-sl-networking
              hsPkgs.cardano-sl-update
              hsPkgs.cardano-sl-util
              hsPkgs.cardano-sl
              hsPkgs.log-warper
              hsPkgs.universum
            ];
            build-tools = [
              hsPkgs.buildPackages.cpphs
            ];
          };
        };
      };
    } // rec { src = ../node; }