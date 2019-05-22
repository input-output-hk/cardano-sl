{ system
, compiler
, flags
, pkgs
, hsPkgs
, pkgconfPkgs
, ... }:
  {
    flags = {
      for-installer = false;
    };
    package = {
      specVersion = "1.10";
      identifier = {
        name = "cardano-sl-tools";
        version = "3.0.2";
      };
      license = "MIT";
      copyright = "2016 IOHK";
      maintainer = "hi@serokell.io";
      author = "Serokell";
      homepage = "";
      url = "";
      synopsis = "Cardano SL - Tools";
      description = "Cardano SL - Tools";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.aeson)
          (hsPkgs.containers)
          (hsPkgs.data-default)
          (hsPkgs.directory)
          (hsPkgs.filepath)
          (hsPkgs.network-transport-tcp)
          (hsPkgs.parsers)
          (hsPkgs.text)
          (hsPkgs.trifecta)
          (hsPkgs.universum)
        ];
      };
      exes = {
        "cardano-genupdate" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.ansi-wl-pprint)
            (hsPkgs.bytestring)
            (hsPkgs.cardano-sl)
            (hsPkgs.cardano-sl-util)
            (hsPkgs.cryptonite)
            (hsPkgs.neat-interpolation)
            (hsPkgs.optparse-applicative)
            (hsPkgs.filepath)
            (hsPkgs.formatting)
            (hsPkgs.process)
            (hsPkgs.tar)
            (hsPkgs.text)
            (hsPkgs.universum)
            (hsPkgs.unix-compat)
          ];
        };
        "cardano-keygen" = {
          depends = pkgs.lib.optionals (!flags.for-installer) [
            (hsPkgs.base)
            (hsPkgs.base58-bytestring)
            (hsPkgs.bytestring)
            (hsPkgs.canonical-json)
            (hsPkgs.cardano-sl)
            (hsPkgs.cardano-sl-chain)
            (hsPkgs.cardano-sl-core)
            (hsPkgs.cardano-sl-crypto)
            (hsPkgs.cardano-sl-util)
            (hsPkgs.directory)
            (hsPkgs.filepath)
            (hsPkgs.formatting)
            (hsPkgs.Glob)
            (hsPkgs.lens)
            (hsPkgs.optparse-applicative)
            (hsPkgs.serokell-util)
            (hsPkgs.text)
            (hsPkgs.universum)
          ];
        };
        "cardano-launcher" = {
          depends = [
            (hsPkgs.aeson)
            (hsPkgs.aeson-options)
            (hsPkgs.ansi-wl-pprint)
            (hsPkgs.async)
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.cardano-report-server)
            (hsPkgs.cardano-sl)
            (hsPkgs.cardano-sl-chain)
            (hsPkgs.cardano-sl-core)
            (hsPkgs.cardano-sl-crypto)
            (hsPkgs.cardano-sl-db)
            (hsPkgs.cardano-sl-infra)
            (hsPkgs.cardano-sl-tools)
            (hsPkgs.cardano-sl-util)
            (hsPkgs.directory)
            (hsPkgs.filepath)
            (hsPkgs.formatting)
            (hsPkgs.lens)
            (hsPkgs.lifted-async)
            (hsPkgs.neat-interpolation)
            (hsPkgs.optparse-applicative)
            (hsPkgs.process)
            (hsPkgs.safe-exceptions)
            (hsPkgs.silently)
            (hsPkgs.text)
            (hsPkgs.time-units)
            (hsPkgs.universum)
            (hsPkgs.unordered-containers)
            (hsPkgs.yaml)
          ] ++ (if !system.isWindows
            then [ (hsPkgs.unix) ]
            else [ (hsPkgs.Win32) ]);
        };
        "cardano-addr-convert" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.ansi-wl-pprint)
            (hsPkgs.cardano-sl)
            (hsPkgs.cardano-sl-core)
            (hsPkgs.cardano-sl-crypto)
            (hsPkgs.cardano-sl-util)
            (hsPkgs.neat-interpolation)
            (hsPkgs.optparse-applicative)
            (hsPkgs.text)
            (hsPkgs.universum)
          ];
        };
        "cardano-cli-docs" = {
          depends = pkgs.lib.optionals (!flags.for-installer) [
            (hsPkgs.base)
            (hsPkgs.cardano-sl)
            (hsPkgs.directory)
            (hsPkgs.filepath)
            (hsPkgs.neat-interpolation)
            (hsPkgs.optparse-applicative)
            (hsPkgs.process)
            (hsPkgs.text)
            (hsPkgs.universum)
          ];
          build-tools = [
            (hsPkgs.buildPackages.cpphs)
          ];
        };
        "cardano-blockchain-analyser" = {
          depends = pkgs.lib.optionals (!flags.for-installer) [
            (hsPkgs.ansi-wl-pprint)
            (hsPkgs.base)
            (hsPkgs.cardano-sl)
            (hsPkgs.cardano-sl-binary)
            (hsPkgs.cardano-sl-chain)
            (hsPkgs.cardano-sl-core)
            (hsPkgs.cardano-sl-crypto)
            (hsPkgs.cardano-sl-db)
            (hsPkgs.cardano-sl-util)
            (hsPkgs.directory)
            (hsPkgs.formatting)
            (hsPkgs.lens)
            (hsPkgs.mtl)
            (hsPkgs.neat-interpolation)
            (hsPkgs.optparse-applicative)
            (hsPkgs.serokell-util)
            (hsPkgs.tabl)
            (hsPkgs.text)
            (hsPkgs.universum)
          ];
          build-tools = [
            (hsPkgs.buildPackages.cpphs)
          ];
        };
        "cardano-x509-certificates" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.cardano-sl-x509)
            (hsPkgs.filepath)
            (hsPkgs.optparse-applicative)
            (hsPkgs.universum)
          ];
        };
        "genesis-hash" = {
          depends = pkgs.lib.optionals (!flags.for-installer) [
            (hsPkgs.base)
            (hsPkgs.universum)
            (hsPkgs.bytestring)
            (hsPkgs.cryptonite)
            (hsPkgs.canonical-json)
          ];
        };
        "wallet-extractor" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.universum)
            (hsPkgs.text)
            (hsPkgs.cardano-sl)
            (hsPkgs.cardano-sl-util)
          ];
        };
      };
      tests = {
        "cardano-sl-tools-test" = {
          depends = [
            (hsPkgs.aeson)
            (hsPkgs.base)
            (hsPkgs.cardano-sl-tools)
            (hsPkgs.cardano-sl-util-test)
            (hsPkgs.directory)
            (hsPkgs.hspec)
            (hsPkgs.temporary)
          ];
          build-tools = [
            (hsPkgs.buildPackages.cpphs)
          ];
        };
      };
    };
  } // rec {
    src = .././../tools;
  }