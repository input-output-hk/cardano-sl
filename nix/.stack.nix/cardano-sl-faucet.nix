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
        name = "cardano-sl-faucet";
        version = "3.0.2";
      };
      license = "MIT";
      copyright = "2018 IOHK";
      maintainer = "ben.ford@tweag.io";
      author = "Ben Ford";
      homepage = "";
      url = "";
      synopsis = "";
      description = "Cardano SL - faucet";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.QuickCheck)
          (hsPkgs.aeson)
          (hsPkgs.bytestring)
          (hsPkgs.cardano-sl-client)
          (hsPkgs.cardano-sl-core)
          (hsPkgs.cardano-sl-crypto)
          (hsPkgs.cardano-sl-util)
          (hsPkgs.cardano-sl-mnemonic)
          (hsPkgs.cardano-wallet)
          (hsPkgs.connection)
          (hsPkgs.cryptonite)
          (hsPkgs.data-default)
          (hsPkgs.directory)
          (hsPkgs.ekg-core)
          (hsPkgs.ekg-statsd)
          (hsPkgs.filepath)
          (hsPkgs.generic-arbitrary)
          (hsPkgs.http-api-data)
          (hsPkgs.http-client)
          (hsPkgs.http-client-tls)
          (hsPkgs.http-types)
          (hsPkgs.lens)
          (hsPkgs.log-warper)
          (hsPkgs.memory)
          (hsPkgs.mmorph)
          (hsPkgs.mtl)
          (hsPkgs.neat-interpolation)
          (hsPkgs.random)
          (hsPkgs.safe-exceptions)
          (hsPkgs.servant)
          (hsPkgs.servant-client-core)
          (hsPkgs.servant-server)
          (hsPkgs.servant-swagger)
          (hsPkgs.servant-swagger-ui)
          (hsPkgs.stm)
          (hsPkgs.swagger2)
          (hsPkgs.tagged)
          (hsPkgs.text)
          (hsPkgs.time)
          (hsPkgs.tls)
          (hsPkgs.universum)
          (hsPkgs.wai)
          (hsPkgs.wai-app-static)
          (hsPkgs.wreq)
        ];
      };
      exes = {
        "cardano-faucet" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.aeson)
            (hsPkgs.bytestring)
            (hsPkgs.cardano-sl-faucet)
            (hsPkgs.cardano-sl-infra)
            (hsPkgs.cardano-sl-util)
            (hsPkgs.ekg)
            (hsPkgs.ekg-statsd)
            (hsPkgs.lens)
            (hsPkgs.log-warper)
            (hsPkgs.mtl)
            (hsPkgs.optparse-applicative)
            (hsPkgs.servant-server)
            (hsPkgs.text)
            (hsPkgs.universum)
            (hsPkgs.warp)
          ];
        };
      };
      tests = {
        "faucet-test" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.QuickCheck)
            (hsPkgs.aeson)
            (hsPkgs.bytestring)
            (hsPkgs.cardano-sl-faucet)
            (hsPkgs.hspec)
            (hsPkgs.time)
            (hsPkgs.universum)
          ];
        };
      };
    };
  } // rec {
    src = .././../faucet;
  }