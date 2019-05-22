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
        name = "cardano-sl-auxx";
        version = "3.0.2";
      };
      license = "MIT";
      copyright = "2016 IOHK";
      maintainer = "hi@serokell.io";
      author = "Serokell";
      homepage = "";
      url = "";
      synopsis = "Cardano SL - Auxx";
      description = "Cardano SL - Auxx";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs.QuickCheck)
          (hsPkgs.Earley)
          (hsPkgs.MonadRandom)
          (hsPkgs.ansi-wl-pprint)
          (hsPkgs.async)
          (hsPkgs.aeson)
          (hsPkgs.base)
          (hsPkgs.bytestring)
          (hsPkgs.canonical-json)
          (hsPkgs.cardano-sl)
          (hsPkgs.cardano-sl-chain)
          (hsPkgs.cardano-sl-chain-test)
          (hsPkgs.cardano-sl-client)
          (hsPkgs.cardano-sl-core)
          (hsPkgs.cardano-sl-core-test)
          (hsPkgs.cardano-sl-crypto)
          (hsPkgs.cardano-sl-db)
          (hsPkgs.cardano-sl-generator)
          (hsPkgs.cardano-sl-infra)
          (hsPkgs.cardano-sl-util)
          (hsPkgs.conduit)
          (hsPkgs.constraints)
          (hsPkgs.containers)
          (hsPkgs.data-default)
          (hsPkgs.formatting)
          (hsPkgs.generic-arbitrary)
          (hsPkgs.haskeline)
          (hsPkgs.lens)
          (hsPkgs.loc)
          (hsPkgs.megaparsec)
          (hsPkgs.mtl)
          (hsPkgs.neat-interpolation)
          (hsPkgs.optparse-applicative)
          (hsPkgs.parser-combinators)
          (hsPkgs.quickcheck-instances)
          (hsPkgs.random)
          (hsPkgs.resourcet)
          (hsPkgs.safe-exceptions)
          (hsPkgs.scientific)
          (hsPkgs.serokell-util)
          (hsPkgs.split)
          (hsPkgs.stm)
          (hsPkgs.text)
          (hsPkgs.formatting)
          (hsPkgs.time-units)
          (hsPkgs.transformers)
          (hsPkgs.universum)
          (hsPkgs.unliftio)
          (hsPkgs.unordered-containers)
          (hsPkgs.validation)
        ] ++ pkgs.lib.optional (!system.isWindows) (hsPkgs.unix);
        build-tools = [
          (hsPkgs.buildPackages.cpphs)
        ];
      };
      exes = {
        "cardano-auxx" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.cardano-sl)
            (hsPkgs.cardano-sl-auxx)
            (hsPkgs.cardano-sl-core)
            (hsPkgs.cardano-sl-db)
            (hsPkgs.cardano-sl-infra)
            (hsPkgs.cardano-sl-networking)
            (hsPkgs.cardano-sl-chain)
            (hsPkgs.cardano-sl-util)
            (hsPkgs.temporary)
            (hsPkgs.network-transport-tcp)
            (hsPkgs.safe-exceptions)
            (hsPkgs.universum)
            (hsPkgs.formatting)
          ];
          build-tools = [
            (hsPkgs.buildPackages.cpphs)
          ];
        };
      };
      tests = {
        "cardano-auxx-test" = {
          depends = [
            (hsPkgs.QuickCheck)
            (hsPkgs.cardano-sl-auxx)
            (hsPkgs.cardano-sl-chain)
            (hsPkgs.cardano-sl-core)
            (hsPkgs.cardano-sl-crypto)
            (hsPkgs.hspec)
            (hsPkgs.universum)
          ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover)
            (hsPkgs.buildPackages.cpphs)
          ];
        };
      };
    };
  } // rec { src = .././../auxx; }