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
        name = "cardano-sl-wallet-tool";
        version = "2.0.0";
      };
      license = "MIT";
      copyright = "2018 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK Engineering Team";
      homepage = "https://github.com/input-output-hk/cardano-sl/#readme";
      url = "";
      synopsis = "A CLI to making API calls on the wallet";
      description = "Please see README.md";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.aeson)
          (hsPkgs.aeson-options)
          (hsPkgs.aeson-pretty)
          (hsPkgs.async)
          (hsPkgs.bifunctors)
          (hsPkgs.bytestring)
          (hsPkgs.cardano-sl)
          (hsPkgs.cardano-sl-crypto)
          (hsPkgs.cardano-sl-core)
          (hsPkgs.cardano-sl-mnemonic)
          (hsPkgs.cardano-sl-util)
          (hsPkgs.cardano-sl-x509)
          (hsPkgs.cardano-sl-wallet)
          (hsPkgs.connection)
          (hsPkgs.containers)
          (hsPkgs.criterion)
          (hsPkgs.cryptonite)
          (hsPkgs.data-default)
          (hsPkgs.directory)
          (hsPkgs.dlist)
          (hsPkgs.exceptions)
          (hsPkgs.formatting)
          (hsPkgs.filepath)
          (hsPkgs.http-api-data)
          (hsPkgs.http-client)
          (hsPkgs.http-client-tls)
          (hsPkgs.http-types)
          (hsPkgs.lens)
          (hsPkgs.mtl)
          (hsPkgs.optparse-applicative)
          (hsPkgs.parsec)
          (hsPkgs.retry)
          (hsPkgs.safe-exceptions)
          (hsPkgs.servant)
          (hsPkgs.servant-client)
          (hsPkgs.servant-client-core)
          (hsPkgs.stm)
          (hsPkgs.text)
          (hsPkgs.time)
          (hsPkgs.time-units)
          (hsPkgs.tls)
          (hsPkgs.transformers)
          (hsPkgs.universum)
          (hsPkgs.unordered-containers)
          (hsPkgs.vector)
          (hsPkgs.x509)
          (hsPkgs.x509-store)
          (hsPkgs.pem)
        ] ++ pkgs.lib.optional (!system.isWindows) (hsPkgs.unix);
      };
      exes = {
        "cardano-sl-wallet-tool" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.cardano-sl-wallet-tool)
            (hsPkgs.optparse-applicative)
            (hsPkgs.universum)
          ];
        };
        "cardano-sl-connect" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.optparse-applicative)
            (hsPkgs.system-filepath)
            (hsPkgs.text)
            (hsPkgs.turtle)
          ];
        };
        "cardano-sl-acceptance-tests" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.async)
            (hsPkgs.process)
            (hsPkgs.system-filepath)
            (hsPkgs.text)
            (hsPkgs.turtle)
          ];
        };
        "sync-plot" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.optparse-applicative)
            (hsPkgs.aeson)
            # (hsPkgs.Chart)
            # (hsPkgs.Chart-cairo)
            (hsPkgs.text)
            (hsPkgs.universum)
            (hsPkgs.vector)
          ];
        };
      };
      tests = {
        "wallet-tool-specs" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.aeson)
            (hsPkgs.bytestring)
            (hsPkgs.cardano-crypto)
            (hsPkgs.cardano-sl)
            (hsPkgs.cardano-sl-client)
            (hsPkgs.cardano-sl-client)
            (hsPkgs.cardano-sl-chain-test)
            (hsPkgs.cardano-sl-core)
            (hsPkgs.cardano-sl-core-test)
            (hsPkgs.cardano-sl-crypto)
            (hsPkgs.cardano-sl-chain)
            (hsPkgs.cardano-sl-util)
            (hsPkgs.cardano-sl-util-test)
            (hsPkgs.cardano-sl-wallet-tool)
            (hsPkgs.cardano-sl-wallet)
            (hsPkgs.data-default)
            (hsPkgs.directory)
            (hsPkgs.formatting)
            (hsPkgs.hedgehog)
            (hsPkgs.hspec)
            (hsPkgs.lens)
            (hsPkgs.QuickCheck)
            (hsPkgs.quickcheck-instances)
            (hsPkgs.safecopy)
            (hsPkgs.safe-exceptions)
            (hsPkgs.servant)
            (hsPkgs.servant-client)
            (hsPkgs.text)
            (hsPkgs.universum)
          ];
        };
      };
    };
  } // rec {
    src = .././../wallet-tool;
  }
