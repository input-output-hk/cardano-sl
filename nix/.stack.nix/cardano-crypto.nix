let
  buildDepError = pkg:
    builtins.throw ''
      The Haskell package set does not contain the package: ${pkg} (build dependency).
      
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
  sysDepError = pkg:
    builtins.throw ''
      The Nixpkgs package set does not contain the package: ${pkg} (system dependency).
      
      You may need to augment the system package mapping in haskell.nix so that it can be found.
      '';
  pkgConfDepError = pkg:
    builtins.throw ''
      The pkg-conf packages does not contain the package: ${pkg} (pkg-conf dependency).
      
      You may need to augment the pkg-conf package mapping in haskell.nix so that it can be found.
      '';
  exeDepError = pkg:
    builtins.throw ''
      The local executable components do not include the component: ${pkg} (executable dependency).
      '';
  legacyExeDepError = pkg:
    builtins.throw ''
      The Haskell package set does not contain the package: ${pkg} (executable dependency).
      
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
  buildToolDepError = pkg:
    builtins.throw ''
      Neither the Haskell package set or the Nixpkgs package set contain the package: ${pkg} (build tool dependency).
      
      If this is a system dependency:
      You may need to augment the system package mapping in haskell.nix so that it can be found.
      
      If this is a Haskell dependency:
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
in { system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { golden-tests = false; golden-tests-exe = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "cardano-crypto"; version = "1.1.0"; };
      license = "MIT";
      copyright = "2016-2017 IOHK";
      maintainer = "contact@typed.io";
      author = "Vincent Hanquez";
      homepage = "https://github.com/input-output-hk/cardano-crypto#readme";
      url = "";
      synopsis = "Cryptography primitives for cardano";
      description = "";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."memory" or (buildDepError "memory"))
          (hsPkgs."deepseq" or (buildDepError "deepseq"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."basement" or (buildDepError "basement"))
          (hsPkgs."foundation" or (buildDepError "foundation"))
          (hsPkgs."cryptonite" or (buildDepError "cryptonite"))
          (hsPkgs."cryptonite-openssl" or (buildDepError "cryptonite-openssl"))
          (hsPkgs."hashable" or (buildDepError "hashable"))
          (hsPkgs."integer-gmp" or (buildDepError "integer-gmp"))
          ];
        buildable = true;
        };
      exes = {
        "golden-tests" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."basement" or (buildDepError "basement"))
            (hsPkgs."foundation" or (buildDepError "foundation"))
            (hsPkgs."memory" or (buildDepError "memory"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."cryptonite" or (buildDepError "cryptonite"))
            (hsPkgs."cardano-crypto" or (buildDepError "cardano-crypto"))
            ] ++ (pkgs.lib).optional (flags.golden-tests-exe) (hsPkgs."inspector" or (buildDepError "inspector"));
          buildable = if flags.golden-tests-exe then true else false;
          };
        };
      tests = {
        "cardano-crypto-test" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."memory" or (buildDepError "memory"))
            (hsPkgs."cryptonite" or (buildDepError "cryptonite"))
            (hsPkgs."cardano-crypto" or (buildDepError "cardano-crypto"))
            (hsPkgs."basement" or (buildDepError "basement"))
            (hsPkgs."foundation" or (buildDepError "foundation"))
            ];
          buildable = true;
          };
        "cardano-crypto-golden-tests" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."basement" or (buildDepError "basement"))
            (hsPkgs."foundation" or (buildDepError "foundation"))
            (hsPkgs."memory" or (buildDepError "memory"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."cryptonite" or (buildDepError "cryptonite"))
            (hsPkgs."cardano-crypto" or (buildDepError "cardano-crypto"))
            ] ++ (pkgs.lib).optional (flags.golden-tests) (hsPkgs."inspector" or (buildDepError "inspector"));
          buildable = if flags.golden-tests then true else false;
          };
        };
      benchmarks = {
        "cardano-crypto-bench" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."memory" or (buildDepError "memory"))
            (hsPkgs."cryptonite" or (buildDepError "cryptonite"))
            (hsPkgs."cardano-crypto" or (buildDepError "cardano-crypto"))
            (hsPkgs."gauge" or (buildDepError "gauge"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-crypto";
      rev = "4590efa638397e952a51a8994b5543e4ea3c1ecd";
      sha256 = "0hl2n3bba5v2j0lmxhs7hs01z3aznh2bwf9cb434icq2g0bl8ms3";
      });
    }