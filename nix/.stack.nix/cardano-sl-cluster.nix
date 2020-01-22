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
      identifier = { name = "cardano-sl-cluster"; version = "3.2.0"; };
      license = "Apache-2.0";
      copyright = "2018 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK Engineering Team";
      homepage = "https://github.com/input-output-hk/cardano-sl/cluster/README.md";
      url = "";
      synopsis = "Utilities to generate and run cluster of nodes";
      description = "See README";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."cardano-sl" or (buildDepError "cardano-sl"))
          (hsPkgs."cardano-sl-chain" or (buildDepError "cardano-sl-chain"))
          (hsPkgs."cardano-sl-core" or (buildDepError "cardano-sl-core"))
          (hsPkgs."cardano-sl-infra" or (buildDepError "cardano-sl-infra"))
          (hsPkgs."cardano-sl-networking" or (buildDepError "cardano-sl-networking"))
          (hsPkgs."cardano-sl-node" or (buildDepError "cardano-sl-node"))
          (hsPkgs."cardano-sl-util" or (buildDepError "cardano-sl-util"))
          (hsPkgs."cardano-sl-x509" or (buildDepError "cardano-sl-x509"))
          (hsPkgs."aeson" or (buildDepError "aeson"))
          (hsPkgs."async" or (buildDepError "async"))
          (hsPkgs."attoparsec" or (buildDepError "attoparsec"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."containers" or (buildDepError "containers"))
          (hsPkgs."directory" or (buildDepError "directory"))
          (hsPkgs."filepath" or (buildDepError "filepath"))
          (hsPkgs."formatting" or (buildDepError "formatting"))
          (hsPkgs."iproute" or (buildDepError "iproute"))
          (hsPkgs."lens" or (buildDepError "lens"))
          (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
          (hsPkgs."megaparsec" or (buildDepError "megaparsec"))
          (hsPkgs."safe" or (buildDepError "safe"))
          (hsPkgs."servant-client" or (buildDepError "servant-client"))
          (hsPkgs."temporary" or (buildDepError "temporary"))
          (hsPkgs."text" or (buildDepError "text"))
          (hsPkgs."time" or (buildDepError "time"))
          (hsPkgs."tls" or (buildDepError "tls"))
          (hsPkgs."universum" or (buildDepError "universum"))
          ];
        buildable = true;
        };
      exes = {
        "cardano-sl-cluster-demo" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."cardano-sl" or (buildDepError "cardano-sl"))
            (hsPkgs."cardano-sl-cluster" or (buildDepError "cardano-sl-cluster"))
            (hsPkgs."cardano-sl-node" or (buildDepError "cardano-sl-node"))
            (hsPkgs."ansi-terminal" or (buildDepError "ansi-terminal"))
            (hsPkgs."async" or (buildDepError "async"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."docopt" or (buildDepError "docopt"))
            (hsPkgs."formatting" or (buildDepError "formatting"))
            (hsPkgs."lens" or (buildDepError "lens"))
            (hsPkgs."universum" or (buildDepError "universum"))
            ];
          buildable = true;
          };
        "cardano-sl-cluster-prepare-environment" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."cardano-sl-cluster" or (buildDepError "cardano-sl-cluster"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."docopt" or (buildDepError "docopt"))
            (hsPkgs."formatting" or (buildDepError "formatting"))
            (hsPkgs."lens" or (buildDepError "lens"))
            (hsPkgs."universum" or (buildDepError "universum"))
            ];
          buildable = true;
          };
        };
      tests = {
        "cardano-sl-cluster-test" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."cardano-sl-cluster" or (buildDepError "cardano-sl-cluster"))
            (hsPkgs."cardano-sl-core" or (buildDepError "cardano-sl-core"))
            (hsPkgs."cardano-sl-infra" or (buildDepError "cardano-sl-infra"))
            (hsPkgs."async" or (buildDepError "async"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."lens" or (buildDepError "lens"))
            (hsPkgs."QuickCheck" or (buildDepError "QuickCheck"))
            (hsPkgs."time" or (buildDepError "time"))
            (hsPkgs."universum" or (buildDepError "universum"))
            ];
          buildable = true;
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././cluster; }