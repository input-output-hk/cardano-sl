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
        name = "cardano-sl-utxo";
        version = "0.1.0.0";
      };
      license = "MIT";
      copyright = "2017-2018 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK Engineering Team";
      homepage = "https://github.com/input-output-hk/cardano-sl/#readme";
      url = "";
      synopsis = "Abstract definitions of UTxO based accounting";
      description = "";
      buildType = "Simple";
    };
    components = {
      "cardano-sl-utxo" = {
        depends  = [
          (hsPkgs.base)
          (hsPkgs.QuickCheck)
          (hsPkgs.cardano-crypto)
          (hsPkgs.cardano-sl)
          (hsPkgs.cardano-sl-binary)
          (hsPkgs.cardano-sl-chain)
          (hsPkgs.cardano-sl-chain-test)
          (hsPkgs.cardano-sl-client)
          (hsPkgs.cardano-sl-core)
          (hsPkgs.cardano-sl-core-test)
          (hsPkgs.cardano-sl-crypto)
          (hsPkgs.cardano-sl-crypto-test)
          (hsPkgs.cardano-sl-db)
          (hsPkgs.cardano-sl-util)
          (hsPkgs.constraints)
          (hsPkgs.containers)
          (hsPkgs.cryptonite)
          (hsPkgs.data-default)
          (hsPkgs.formatting)
          (hsPkgs.lens)
          (hsPkgs.log-warper)
          (hsPkgs.mtl)
          (hsPkgs.reflection)
          (hsPkgs.safecopy)
          (hsPkgs.safe-exceptions)
          (hsPkgs.serokell-util)
          (hsPkgs.universum)
          (hsPkgs.unordered-containers)
          (hsPkgs.vector)
        ];
      };
    };
  } // rec { src = .././../utxo; }