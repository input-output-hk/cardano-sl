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
        name = "cardano-sl-node";
        version = "3.0.2";
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
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.aeson)
          (hsPkgs.async)
          (hsPkgs.bytestring)
          (hsPkgs.cardano-sl)
          (hsPkgs.cardano-sl-chain)
          (hsPkgs.cardano-sl-core)
          (hsPkgs.cardano-sl-crypto)
          (hsPkgs.cardano-sl-db)
          (hsPkgs.cardano-sl-infra)
          (hsPkgs.cardano-sl-networking)
          (hsPkgs.cardano-sl-node-ipc)
          (hsPkgs.cardano-sl-util)
          (hsPkgs.cardano-sl-x509)
          (hsPkgs.connection)
          (hsPkgs.data-default)
          (hsPkgs.http-client)
          (hsPkgs.http-client-tls)
          (hsPkgs.http-media)
          (hsPkgs.http-types)
          (hsPkgs.lens)
          (hsPkgs.serokell-util)
          (hsPkgs.servant-client)
          (hsPkgs.servant-server)
          (hsPkgs.servant-swagger)
          (hsPkgs.servant-swagger-ui)
          (hsPkgs.stm)
          (hsPkgs.swagger2)
          (hsPkgs.text)
          (hsPkgs.time-units)
          (hsPkgs.tls)
          (hsPkgs.universum)
          (hsPkgs.wai)
          (hsPkgs.warp)
          (hsPkgs.x509)
          (hsPkgs.x509-store)
        ];
      };
      exes = {
        "cardano-node-simple" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.cardano-sl-chain)
            (hsPkgs.cardano-sl-core)
            (hsPkgs.cardano-sl-util)
            (hsPkgs.cardano-sl-node)
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
          depends = [
            (hsPkgs.base)
            (hsPkgs.HUnit)
            (hsPkgs.QuickCheck)
            (hsPkgs.cardano-sl-core)
            (hsPkgs.cardano-sl-utxo)
            (hsPkgs.containers)
            (hsPkgs.data-default)
            (hsPkgs.hashable)
            (hsPkgs.hspec)
            (hsPkgs.lens)
            (hsPkgs.mtl)
            (hsPkgs.text)
            (hsPkgs.universum)
            (hsPkgs.validation)
          ];
        };
      };
    };
  } // rec { src = .././../node; }