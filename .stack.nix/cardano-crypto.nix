{ compiler, flags ? {}, hsPkgs, pkgconfPkgs, pkgs, system }:
let
    _flags = {} // flags;
    in {
      flags = _flags;
      package = {
        specVersion = "1.10";
        identifier = {
          name = "cardano-crypto";
          version = "1.0.0";
        };
        license = "MIT";
        copyright = "2016-2017 IOHK";
        maintainer = "contact@typed.io";
        author = "Vincent Hanquez";
        homepage = "https://github.com/input-output-hk/cardano-crypto#readme";
        url = "";
        synopsis = "Cryptography primitives for cardano";
        description = "";
        buildType = "Simple";
      };
      components = {
        cardano-crypto = {
          depends  = [
            hsPkgs.base
            hsPkgs.memory
            hsPkgs.deepseq
            hsPkgs.bytestring
            hsPkgs.cryptonite
            hsPkgs.cryptonite-openssl
            hsPkgs.hashable
          ];
        };
        tests = {
          cardano-crypto-test = {
            depends  = [
              hsPkgs.base
              hsPkgs.bytestring
              hsPkgs.memory
              hsPkgs.cryptonite
              hsPkgs.cardano-crypto
              hsPkgs.tasty
              hsPkgs.tasty-quickcheck
            ];
          };
        };
      };
    } // {
      src = pkgs.fetchgit {
        url = "https://github.com/input-output-hk/cardano-crypto";
        rev = "287cc575fafe86af9d24af9d012c47f9d3f04da0";
        sha256 = null;
      };
    }