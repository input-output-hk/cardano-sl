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
        name = "cardano-sl-explorer";
        version = "3.0.2";
      };
      license = "MIT";
      copyright = "2017 IOHK";
      maintainer = "IOHK <support@iohk.io>";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Cardano explorer";
      description = "Please see README.md";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs.QuickCheck)
          (hsPkgs.aeson)
          (hsPkgs.base)
          (hsPkgs.bytestring)
          (hsPkgs.conduit)
          (hsPkgs.containers)
          (hsPkgs.data-default)
          (hsPkgs.ether)
          (hsPkgs.exceptions)
          (hsPkgs.formatting)
          (hsPkgs.free)
          (hsPkgs.lens)
          (hsPkgs.memory)
          (hsPkgs.mmorph)
          (hsPkgs.resourcet)
          (hsPkgs.rocksdb-haskell-ng)
          (hsPkgs.safe-exceptions)
          (hsPkgs.serokell-util)
          (hsPkgs.stm)
          (hsPkgs.text)
          (hsPkgs.formatting)
          (hsPkgs.time)
          (hsPkgs.time-units)
          (hsPkgs.transformers)
          (hsPkgs.universum)
          (hsPkgs.unliftio)
          (hsPkgs.unordered-containers)
          (hsPkgs.vector)
          (hsPkgs.cardano-sl)
          (hsPkgs.cardano-sl-binary)
          (hsPkgs.cardano-sl-chain)
          (hsPkgs.cardano-sl-chain-test)
          (hsPkgs.cardano-sl-core)
          (hsPkgs.cardano-sl-crypto)
          (hsPkgs.cardano-sl-crypto-test)
          (hsPkgs.cardano-sl-db)
          (hsPkgs.cardano-sl-generator)
          (hsPkgs.cardano-sl-infra)
          (hsPkgs.cardano-sl-util)
          (hsPkgs.mtl)
          (hsPkgs.servant)
          (hsPkgs.servant-server)
          (hsPkgs.http-types)
          (hsPkgs.socket-io)
          (hsPkgs.engine-io)
          (hsPkgs.engine-io-wai)
          (hsPkgs.wai)
          (hsPkgs.wai-extra)
          (hsPkgs.wai-cors)
          (hsPkgs.warp)
        ];
        build-tools = [
          (hsPkgs.buildPackages.cpphs)
        ];
      };
      exes = {
        "cardano-explorer" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.cardano-sl)
            (hsPkgs.cardano-sl-chain)
            (hsPkgs.cardano-sl-core)
            (hsPkgs.cardano-sl-infra)
            (hsPkgs.cardano-sl-explorer)
            (hsPkgs.cardano-sl-util)
            (hsPkgs.optparse-applicative)
            (hsPkgs.universum)
          ];
          build-tools = [
            (hsPkgs.buildPackages.cpphs)
          ];
        };
        "cardano-explorer-hs2purs" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.cardano-sl-explorer)
            (hsPkgs.purescript-bridge)
            (hsPkgs.universum)
            (hsPkgs.optparse-simple)
          ];
          build-tools = [
            (hsPkgs.buildPackages.cpphs)
          ];
        };
        "cardano-explorer-swagger" = {
          depends = [
            (hsPkgs.aeson)
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.cardano-sl-explorer)
            (hsPkgs.lens)
            (hsPkgs.optparse-applicative)
            (hsPkgs.servant-multipart)
            (hsPkgs.servant-server)
            (hsPkgs.servant-swagger)
            (hsPkgs.swagger2)
            (hsPkgs.universum)
          ];
          build-tools = [
            (hsPkgs.buildPackages.cpphs)
          ];
        };
        "cardano-explorer-mock" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.cardano-sl-explorer)
            (hsPkgs.optparse-applicative)
            (hsPkgs.universum)
          ];
          build-tools = [
            (hsPkgs.buildPackages.cpphs)
          ];
        };
      };
      tests = {
        "cardano-explorer-test" = {
          depends = [
            (hsPkgs.QuickCheck)
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.cardano-sl)
            (hsPkgs.cardano-sl-binary-test)
            (hsPkgs.cardano-sl-chain-test)
            (hsPkgs.cardano-sl-core)
            (hsPkgs.cardano-sl-core-test)
            (hsPkgs.cardano-sl-crypto)
            (hsPkgs.cardano-sl-explorer)
            (hsPkgs.cardano-sl-chain)
            (hsPkgs.cardano-sl-util)
            (hsPkgs.containers)
            (hsPkgs.cryptonite)
            (hsPkgs.engine-io)
            (hsPkgs.generic-arbitrary)
            (hsPkgs.hspec)
            (hsPkgs.lens)
            (hsPkgs.universum)
            (hsPkgs.warp)
          ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover)
            (hsPkgs.buildPackages.cpphs)
          ];
        };
      };
      benchmarks = {
        "cardano-explorer-bench" = {
          depends = [
            (hsPkgs.QuickCheck)
            (hsPkgs.base)
            (hsPkgs.cardano-sl)
            (hsPkgs.cardano-sl-chain-test)
            (hsPkgs.cardano-sl-explorer)
            (hsPkgs.criterion)
            (hsPkgs.universum)
            (hsPkgs.weigh)
          ];
          build-tools = [
            (hsPkgs.buildPackages.cpphs)
          ];
        };
      };
    };
  } // rec {
    src = .././../explorer;
  }