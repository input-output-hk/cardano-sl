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
      identifier = { name = "cardano-sl-script-runner"; version = "3.2.0"; };
      license = "Apache-2.0";
      copyright = "2018 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Cardano SL - Script Runner";
      description = "Cardano SL - ScriptRunner";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."aeson" or (buildDepError "aeson"))
          (hsPkgs."brick" or (buildDepError "brick"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."cardano-sl" or (buildDepError "cardano-sl"))
          (hsPkgs."cardano-sl-chain" or (buildDepError "cardano-sl-chain"))
          (hsPkgs."cardano-sl-client" or (buildDepError "cardano-sl-client"))
          (hsPkgs."cardano-sl-core" or (buildDepError "cardano-sl-core"))
          (hsPkgs."cardano-sl-crypto" or (buildDepError "cardano-sl-crypto"))
          (hsPkgs."cardano-sl-db" or (buildDepError "cardano-sl-db"))
          (hsPkgs."cardano-sl-infra" or (buildDepError "cardano-sl-infra"))
          (hsPkgs."cardano-sl-networking" or (buildDepError "cardano-sl-networking"))
          (hsPkgs."cardano-sl-util" or (buildDepError "cardano-sl-util"))
          (hsPkgs."conduit" or (buildDepError "conduit"))
          (hsPkgs."constraints" or (buildDepError "constraints"))
          (hsPkgs."containers" or (buildDepError "containers"))
          (hsPkgs."data-default" or (buildDepError "data-default"))
          (hsPkgs."dns" or (buildDepError "dns"))
          (hsPkgs."formatting" or (buildDepError "formatting"))
          (hsPkgs."lens" or (buildDepError "lens"))
          (hsPkgs."lifted-async" or (buildDepError "lifted-async"))
          (hsPkgs."mtl" or (buildDepError "mtl"))
          (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
          (hsPkgs."process" or (buildDepError "process"))
          (hsPkgs."resourcet" or (buildDepError "resourcet"))
          (hsPkgs."serokell-util" or (buildDepError "serokell-util"))
          (hsPkgs."stm" or (buildDepError "stm"))
          (hsPkgs."text" or (buildDepError "text"))
          (hsPkgs."time" or (buildDepError "time"))
          (hsPkgs."time-units" or (buildDepError "time-units"))
          (hsPkgs."turtle" or (buildDepError "turtle"))
          (hsPkgs."universum" or (buildDepError "universum"))
          (hsPkgs."unix" or (buildDepError "unix"))
          (hsPkgs."unordered-containers" or (buildDepError "unordered-containers"))
          (hsPkgs."vector" or (buildDepError "vector"))
          (hsPkgs."vty" or (buildDepError "vty"))
          (hsPkgs."yaml" or (buildDepError "yaml"))
          ];
        buildable = if system.isWindows then false else true;
        };
      exes = {
        "testcases" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."cardano-sl" or (buildDepError "cardano-sl"))
            (hsPkgs."cardano-sl-binary" or (buildDepError "cardano-sl-binary"))
            (hsPkgs."cardano-sl-chain" or (buildDepError "cardano-sl-chain"))
            (hsPkgs."cardano-sl-core" or (buildDepError "cardano-sl-core"))
            (hsPkgs."cardano-sl-db" or (buildDepError "cardano-sl-db"))
            (hsPkgs."cardano-sl-infra" or (buildDepError "cardano-sl-infra"))
            (hsPkgs."cardano-sl-script-runner" or (buildDepError "cardano-sl-script-runner"))
            (hsPkgs."cardano-sl-util" or (buildDepError "cardano-sl-util"))
            (hsPkgs."cborg" or (buildDepError "cborg"))
            (hsPkgs."constraints" or (buildDepError "constraints"))
            (hsPkgs."data-default" or (buildDepError "data-default"))
            (hsPkgs."formatting" or (buildDepError "formatting"))
            (hsPkgs."serokell-util" or (buildDepError "serokell-util"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."time-units" or (buildDepError "time-units"))
            (hsPkgs."turtle" or (buildDepError "turtle"))
            (hsPkgs."universum" or (buildDepError "universum"))
            ];
          buildable = if system.isWindows then false else true;
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././script-runner; }