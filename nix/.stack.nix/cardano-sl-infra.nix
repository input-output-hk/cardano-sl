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
        name = "cardano-sl-infra";
        version = "3.0.2";
      };
      license = "MIT";
      copyright = "2016 IOHK";
      maintainer = "hi@serokell.io";
      author = "Serokell";
      homepage = "";
      url = "";
      synopsis = "Cardano SL - infrastructural";
      description = "Cardano SL - infrastructural";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs.aeson)
          (hsPkgs.async)
          (hsPkgs.base)
          (hsPkgs.parsec)
          (hsPkgs.base64-bytestring)
          (hsPkgs.bytestring)
          (hsPkgs.cardano-sl-binary)
          (hsPkgs.cardano-sl-chain)
          (hsPkgs.cardano-sl-core)
          (hsPkgs.cardano-sl-crypto)
          (hsPkgs.cardano-sl-db)
          (hsPkgs.cardano-sl-networking)
          (hsPkgs.cardano-sl-util)
          (hsPkgs.cardano-report-server)
          (hsPkgs.clock)
          (hsPkgs.conduit)
          (hsPkgs.containers)
          (hsPkgs.directory)
          (hsPkgs.dns)
          (hsPkgs.ekg-core)
          (hsPkgs.ekg-statsd)
          (hsPkgs.ekg-wai)
          (hsPkgs.ether)
          (hsPkgs.exceptions)
          (hsPkgs.filepath)
          (hsPkgs.formatting)
          (hsPkgs.hashable)
          (hsPkgs.http-client)
          (hsPkgs.http-client-tls)
          (hsPkgs.iproute)
          (hsPkgs.kademlia)
          (hsPkgs.lens)
          (hsPkgs.mtl)
          (hsPkgs.network-info)
          (hsPkgs.network-transport)
          (hsPkgs.network-transport-tcp)
          (hsPkgs.lzma-conduit)
          (hsPkgs.optparse-applicative)
          (hsPkgs.safe-exceptions)
          (hsPkgs.serokell-util)
          (hsPkgs.stm)
          (hsPkgs.tar)
          (hsPkgs.time)
          (hsPkgs.tagged)
          (hsPkgs.vector)
          (hsPkgs.text)
          (hsPkgs.time-units)
          (hsPkgs.network-transport)
          (hsPkgs.universum)
          (hsPkgs.unliftio)
          (hsPkgs.unordered-containers)
          (hsPkgs.yaml)
        ] ++ pkgs.lib.optional (!system.isWindows) (hsPkgs.unix);
        build-tools = [
          (hsPkgs.buildPackages.cpphs)
        ];
      };
      tests = {
        "infra-test" = {
          depends = [
            (hsPkgs.QuickCheck)
            (hsPkgs.async)
            (hsPkgs.aeson)
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.cardano-sl-binary-test)
            (hsPkgs.cardano-sl-chain)
            (hsPkgs.cardano-sl-chain-test)
            (hsPkgs.cardano-sl-core)
            (hsPkgs.cardano-sl-core-test)
            (hsPkgs.cardano-sl-crypto)
            (hsPkgs.cardano-sl-crypto-test)
            (hsPkgs.cardano-sl-infra)
            (hsPkgs.cardano-sl-networking)
            (hsPkgs.cardano-sl-util-test)
            (hsPkgs.containers)
            (hsPkgs.dns)
            (hsPkgs.generic-arbitrary)
            (hsPkgs.hedgehog)
            (hsPkgs.hspec)
            (hsPkgs.iproute)
            (hsPkgs.kademlia)
            (hsPkgs.universum)
            (hsPkgs.yaml)
          ];
        };
      };
    };
  } // rec {
    src = .././../infra;
  }