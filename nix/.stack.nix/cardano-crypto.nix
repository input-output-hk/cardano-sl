{ system
, compiler
, flags
, pkgs
, hsPkgs
, pkgconfPkgs
, ... }:
  {
    flags = {
      golden-tests = false;
      golden-tests-exe = false;
    };
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
      "library" = {
        depends  = [
          (hsPkgs.base)
          (hsPkgs.memory)
          (hsPkgs.deepseq)
          (hsPkgs.bytestring)
          (hsPkgs.basement)
          (hsPkgs.foundation)
          (hsPkgs.cryptonite)
          (hsPkgs.cryptonite-openssl)
          (hsPkgs.hashable)
          (hsPkgs.integer-gmp)
        ];
      };
      exes = {
        "golden-tests" = {
          depends  = [
            (hsPkgs.base)
            (hsPkgs.basement)
            (hsPkgs.foundation)
            (hsPkgs.memory)
            (hsPkgs.bytestring)
            (hsPkgs.cryptonite)
            (hsPkgs.cardano-crypto)
          ] ++ pkgs.lib.optional (flags.golden-tests-exe) (hsPkgs.inspector);
        };
      };
      tests = {
        "cardano-crypto-test" = {
          depends  = [
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.memory)
            (hsPkgs.cryptonite)
            (hsPkgs.cardano-crypto)
            (hsPkgs.basement)
            (hsPkgs.foundation)
          ];
        };
        "cardano-crypto-golden-tests" = {
          depends  = [
            (hsPkgs.base)
            (hsPkgs.basement)
            (hsPkgs.foundation)
            (hsPkgs.memory)
            (hsPkgs.bytestring)
            (hsPkgs.cryptonite)
            (hsPkgs.cardano-crypto)
          ] ++ pkgs.lib.optional (flags.golden-tests) (hsPkgs.inspector);
        };
      };
      benchmarks = {
        "cardano-crypto-bench" = {
          depends  = [
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.memory)
            (hsPkgs.cryptonite)
            (hsPkgs.cardano-crypto)
            (hsPkgs.gauge)
          ];
        };
      };
    };
  } // {
    src = pkgs.fetchgit {
      url = "https://github.com/angerman/cardano-crypto";
      rev = "1e436fe8b69e7c8b399937db394d99229fcd775c";
      sha256 = "02kisrjavvi4hr2kyq7znd7y3y5c4pbl6w8gr5a58fl03m69sngr";
    };
  }