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
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "cardano-sl-generator"; version = "3.2.0"; };
      license = "Apache-2.0";
      copyright = "2017 IOHK";
      maintainer = "hi@serokell.io";
      author = "Serokell";
      homepage = "";
      url = "";
      synopsis = "Cardano SL - arbitrary data generation";
      description = "Cardano SL - arbitrary data generation";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."MonadRandom" or (buildDepError "MonadRandom"))
          (hsPkgs."QuickCheck" or (buildDepError "QuickCheck"))
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."cardano-sl" or (buildDepError "cardano-sl"))
          (hsPkgs."cardano-sl-chain" or (buildDepError "cardano-sl-chain"))
          (hsPkgs."cardano-sl-chain-test" or (buildDepError "cardano-sl-chain-test"))
          (hsPkgs."cardano-sl-client" or (buildDepError "cardano-sl-client"))
          (hsPkgs."cardano-sl-core" or (buildDepError "cardano-sl-core"))
          (hsPkgs."cardano-sl-core-test" or (buildDepError "cardano-sl-core-test"))
          (hsPkgs."cardano-sl-crypto" or (buildDepError "cardano-sl-crypto"))
          (hsPkgs."cardano-sl-db" or (buildDepError "cardano-sl-db"))
          (hsPkgs."cardano-sl-infra" or (buildDepError "cardano-sl-infra"))
          (hsPkgs."cardano-sl-util" or (buildDepError "cardano-sl-util"))
          (hsPkgs."containers" or (buildDepError "containers"))
          (hsPkgs."cryptonite" or (buildDepError "cryptonite"))
          (hsPkgs."data-default" or (buildDepError "data-default"))
          (hsPkgs."ether" or (buildDepError "ether"))
          (hsPkgs."exceptions" or (buildDepError "exceptions"))
          (hsPkgs."formatting" or (buildDepError "formatting"))
          (hsPkgs."lens" or (buildDepError "lens"))
          (hsPkgs."monad-control" or (buildDepError "monad-control"))
          (hsPkgs."random" or (buildDepError "random"))
          (hsPkgs."safe-exceptions" or (buildDepError "safe-exceptions"))
          (hsPkgs."serokell-util" or (buildDepError "serokell-util"))
          (hsPkgs."text" or (buildDepError "text"))
          (hsPkgs."formatting" or (buildDepError "formatting"))
          (hsPkgs."time-units" or (buildDepError "time-units"))
          (hsPkgs."transformers-base" or (buildDepError "transformers-base"))
          (hsPkgs."universum" or (buildDepError "universum"))
          (hsPkgs."unliftio" or (buildDepError "unliftio"))
          (hsPkgs."unordered-containers" or (buildDepError "unordered-containers"))
          (hsPkgs."vector" or (buildDepError "vector"))
          ];
        build-tools = [
          (hsPkgs.buildPackages.cpphs or (pkgs.buildPackages.cpphs or (buildToolDepError "cpphs")))
          ];
        buildable = true;
        };
      exes = {
        "cardano-sl-verification-bench-exe" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."cardano-sl" or (buildDepError "cardano-sl"))
            (hsPkgs."cardano-sl-binary" or (buildDepError "cardano-sl-binary"))
            (hsPkgs."cardano-sl-chain" or (buildDepError "cardano-sl-chain"))
            (hsPkgs."cardano-sl-core" or (buildDepError "cardano-sl-core"))
            (hsPkgs."cardano-sl-crypto" or (buildDepError "cardano-sl-crypto"))
            (hsPkgs."cardano-sl-db" or (buildDepError "cardano-sl-db"))
            (hsPkgs."cardano-sl-generator" or (buildDepError "cardano-sl-generator"))
            (hsPkgs."cardano-sl-util" or (buildDepError "cardano-sl-util"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."deepseq" or (buildDepError "deepseq"))
            (hsPkgs."directory" or (buildDepError "directory"))
            (hsPkgs."formatting" or (buildDepError "formatting"))
            (hsPkgs."MonadRandom" or (buildDepError "MonadRandom"))
            (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
            (hsPkgs."random" or (buildDepError "random"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."time-units" or (buildDepError "time-units"))
            (hsPkgs."universum" or (buildDepError "universum"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.cpphs or (pkgs.buildPackages.cpphs or (buildToolDepError "cpphs")))
            ];
          buildable = true;
          };
        };
      tests = {
        "cardano-generator-test" = {
          depends = [
            (hsPkgs."MonadRandom" or (buildDepError "MonadRandom"))
            (hsPkgs."QuickCheck" or (buildDepError "QuickCheck"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."cardano-sl" or (buildDepError "cardano-sl"))
            (hsPkgs."cardano-sl-binary" or (buildDepError "cardano-sl-binary"))
            (hsPkgs."cardano-sl-chain" or (buildDepError "cardano-sl-chain"))
            (hsPkgs."cardano-sl-chain-test" or (buildDepError "cardano-sl-chain-test"))
            (hsPkgs."cardano-sl-core" or (buildDepError "cardano-sl-core"))
            (hsPkgs."cardano-sl-crypto" or (buildDepError "cardano-sl-crypto"))
            (hsPkgs."cardano-sl-db" or (buildDepError "cardano-sl-db"))
            (hsPkgs."cardano-sl-generator" or (buildDepError "cardano-sl-generator"))
            (hsPkgs."cardano-sl-util" or (buildDepError "cardano-sl-util"))
            (hsPkgs."cardano-sl-util-test" or (buildDepError "cardano-sl-util-test"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."data-default" or (buildDepError "data-default"))
            (hsPkgs."formatting" or (buildDepError "formatting"))
            (hsPkgs."hspec" or (buildDepError "hspec"))
            (hsPkgs."lens" or (buildDepError "lens"))
            (hsPkgs."safe-exceptions" or (buildDepError "safe-exceptions"))
            (hsPkgs."serokell-util" or (buildDepError "serokell-util"))
            (hsPkgs."universum" or (buildDepError "universum"))
            (hsPkgs."unordered-containers" or (buildDepError "unordered-containers"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover or (pkgs.buildPackages.hspec-discover or (buildToolDepError "hspec-discover")))
            (hsPkgs.buildPackages.cpphs or (pkgs.buildPackages.cpphs or (buildToolDepError "cpphs")))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "cardano-sl-verification-bench" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."cardano-sl" or (buildDepError "cardano-sl"))
            (hsPkgs."cardano-sl-chain" or (buildDepError "cardano-sl-chain"))
            (hsPkgs."cardano-sl-chain-test" or (buildDepError "cardano-sl-chain-test"))
            (hsPkgs."cardano-sl-core" or (buildDepError "cardano-sl-core"))
            (hsPkgs."cardano-sl-crypto" or (buildDepError "cardano-sl-crypto"))
            (hsPkgs."cardano-sl-db" or (buildDepError "cardano-sl-db"))
            (hsPkgs."cardano-sl-generator" or (buildDepError "cardano-sl-generator"))
            (hsPkgs."cardano-sl-util" or (buildDepError "cardano-sl-util"))
            (hsPkgs."criterion" or (buildDepError "criterion"))
            (hsPkgs."MonadRandom" or (buildDepError "MonadRandom"))
            (hsPkgs."random" or (buildDepError "random"))
            (hsPkgs."time-units" or (buildDepError "time-units"))
            (hsPkgs."universum" or (buildDepError "universum"))
            (hsPkgs."serokell-util" or (buildDepError "serokell-util"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.cpphs or (pkgs.buildPackages.cpphs or (buildToolDepError "cpphs")))
            ];
          buildable = true;
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././generator; }