{ compiler, flags ? {}, hsPkgs, pkgconfPkgs, pkgs, system }:
let
    _flags = {
      golden-tests = false;
    } // flags;
    in {
      flags = _flags;
      package = {
        specVersion = "1.10";
        identifier = {
          name = "cardano-crypto";
          version = "1.1.0";
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
            hsPkgs.basement
            hsPkgs.foundation
            hsPkgs.cryptonite
            hsPkgs.cryptonite-openssl
            hsPkgs.hashable
            hsPkgs.integer-gmp
          ];
        };
        exes = {
          golden-tests = {
            depends  = [
              hsPkgs.base
              hsPkgs.basement
              hsPkgs.foundation
              hsPkgs.memory
              hsPkgs.bytestring
              hsPkgs.cryptonite
              hsPkgs.cardano-crypto
            ] ++ pkgs.lib.optional _flags.golden-tests hsPkgs.inspector;
          };
        };
        tests = {
          cardano-crypto-test = {
            depends  = [
              hsPkgs.base
              hsPkgs.bytestring
              hsPkgs.memory
              hsPkgs.cryptonite
              hsPkgs.cardano-crypto
              hsPkgs.basement
              hsPkgs.foundation
            ];
          };
          cardano-crypto-golden-tests = {
            depends  = [
              hsPkgs.base
              hsPkgs.basement
              hsPkgs.foundation
              hsPkgs.memory
              hsPkgs.bytestring
              hsPkgs.cryptonite
              hsPkgs.inspector
              hsPkgs.cardano-crypto
            ];
          };
        };
        benchmarks = {
          cardano-crypto-bench = {
            depends  = [
              hsPkgs.base
              hsPkgs.bytestring
              hsPkgs.memory
              hsPkgs.cryptonite
              hsPkgs.cardano-crypto
              hsPkgs.gauge
            ];
          };
        };
      };
    } // {
      src = pkgs.fetchgit {
        url = "https://github.com/input-output-hk/cardano-crypto";
        rev = "19257956ec6563fa650e591ebd6250acf95a40c9";
        sha256 = null;
      };
    }