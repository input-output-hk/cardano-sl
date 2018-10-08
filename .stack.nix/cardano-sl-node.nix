{ system
, compiler
, flags ? {}
, pkgs
, hsPkgs
, pkgconfPkgs }:
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
      description = "Provides a 'cardano-node-simple' executable which can\nconnect to the Cardano network and act as a full node\nbut does not have any wallet capabilities.";
      buildType = "Simple";
    };
    components = {
      exes = {
        "cardano-node-simple" = {
          depends  = [
            (hsPkgs.base)
            (hsPkgs.cardano-sl-core)
            (hsPkgs.cardano-sl-chain)
            (hsPkgs.cardano-sl-db)
            (hsPkgs.cardano-sl-networking)
            (hsPkgs.cardano-sl-util)
            (hsPkgs.cardano-sl)
            (hsPkgs.universum)
          ];
          build-tools = [
            (hsPkgs.buildPackages.cpphs)
          ];
        };
      };
      tests = {
        "property-tests" = {
          depends  = [
            (hsPkgs.base)
            (hsPkgs.HUnit)
            (hsPkgs.QuickCheck)
            (hsPkgs.cardano-sl)
            (hsPkgs.cardano-sl-chain)
            (hsPkgs.cardano-sl-client)
            (hsPkgs.cardano-sl-core)
            (hsPkgs.cardano-sl-crypto)
            (hsPkgs.cardano-sl-db)
            (hsPkgs.cardano-sl-infra)
            (hsPkgs.cardano-sl-networking)
            (hsPkgs.cardano-sl-util)
            (hsPkgs.cardano-sl-utxo)
            (hsPkgs.cardano-sl-wallet-new)
            (hsPkgs.constraints)
            (hsPkgs.containers)
            (hsPkgs.data-default)
            (hsPkgs.hashable)
            (hsPkgs.hspec)
            (hsPkgs.lens)
            (hsPkgs.mtl)
            (hsPkgs.reflection)
            (hsPkgs.safe-exceptions)
            (hsPkgs.text)
            (hsPkgs.universum)
            (hsPkgs.validation)
          ];
        };
      };
    };
  } // rec { src = ../node; }