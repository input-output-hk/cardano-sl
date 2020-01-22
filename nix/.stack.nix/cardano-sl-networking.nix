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
    flags = { benchmarks = false; };
    package = {
      specVersion = "1.20";
      identifier = { name = "cardano-sl-networking"; version = "3.2.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "";
      author = "";
      homepage = "";
      url = "";
      synopsis = "";
      description = "";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (buildDepError "aeson"))
          (hsPkgs."aeson-options" or (buildDepError "aeson-options"))
          (hsPkgs."async" or (buildDepError "async"))
          (hsPkgs."attoparsec" or (buildDepError "attoparsec"))
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."binary" or (buildDepError "binary"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."cardano-sl-chain" or (buildDepError "cardano-sl-chain"))
          (hsPkgs."cardano-sl-util" or (buildDepError "cardano-sl-util"))
          (hsPkgs."containers" or (buildDepError "containers"))
          (hsPkgs."ekg-core" or (buildDepError "ekg-core"))
          (hsPkgs."formatting" or (buildDepError "formatting"))
          (hsPkgs."formatting" or (buildDepError "formatting"))
          (hsPkgs."hashable" or (buildDepError "hashable"))
          (hsPkgs."lens" or (buildDepError "lens"))
          (hsPkgs."mtl" or (buildDepError "mtl"))
          (hsPkgs."mtl" or (buildDepError "mtl"))
          (hsPkgs."network" or (buildDepError "network"))
          (hsPkgs."network-transport" or (buildDepError "network-transport"))
          (hsPkgs."network-transport-tcp" or (buildDepError "network-transport-tcp"))
          (hsPkgs."random" or (buildDepError "random"))
          (hsPkgs."safe-exceptions" or (buildDepError "safe-exceptions"))
          (hsPkgs."scientific" or (buildDepError "scientific"))
          (hsPkgs."stm" or (buildDepError "stm"))
          (hsPkgs."text" or (buildDepError "text"))
          (hsPkgs."these" or (buildDepError "these"))
          (hsPkgs."time" or (buildDepError "time"))
          (hsPkgs."time-units" or (buildDepError "time-units"))
          (hsPkgs."universum" or (buildDepError "universum"))
          (hsPkgs."unordered-containers" or (buildDepError "unordered-containers"))
          ];
        buildable = true;
        };
      exes = {
        "ping-pong" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."async" or (buildDepError "async"))
            (hsPkgs."binary" or (buildDepError "binary"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."cardano-sl-networking" or (buildDepError "cardano-sl-networking"))
            (hsPkgs."cardano-sl-util" or (buildDepError "cardano-sl-util"))
            (hsPkgs."contravariant" or (buildDepError "contravariant"))
            (hsPkgs."network-transport" or (buildDepError "network-transport"))
            (hsPkgs."network-transport-tcp" or (buildDepError "network-transport-tcp"))
            (hsPkgs."random" or (buildDepError "random"))
            ];
          buildable = true;
          };
        "bench-sender" = {
          depends = [
            (hsPkgs."async" or (buildDepError "async"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."cardano-sl-util" or (buildDepError "cardano-sl-util"))
            (hsPkgs."contravariant" or (buildDepError "contravariant"))
            (hsPkgs."lens" or (buildDepError "lens"))
            (hsPkgs."MonadRandom" or (buildDepError "MonadRandom"))
            (hsPkgs."mtl" or (buildDepError "mtl"))
            (hsPkgs."network-transport" or (buildDepError "network-transport"))
            (hsPkgs."network-transport-tcp" or (buildDepError "network-transport-tcp"))
            (hsPkgs."optparse-simple" or (buildDepError "optparse-simple"))
            (hsPkgs."random" or (buildDepError "random"))
            (hsPkgs."safe-exceptions" or (buildDepError "safe-exceptions"))
            (hsPkgs."serokell-util" or (buildDepError "serokell-util"))
            (hsPkgs."time" or (buildDepError "time"))
            (hsPkgs."time-units" or (buildDepError "time-units"))
            ];
          buildable = if flags.benchmarks then true else false;
          };
        "bench-receiver" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."cardano-sl-util" or (buildDepError "cardano-sl-util"))
            (hsPkgs."contravariant" or (buildDepError "contravariant"))
            (hsPkgs."network-transport-tcp" or (buildDepError "network-transport-tcp"))
            (hsPkgs."optparse-simple" or (buildDepError "optparse-simple"))
            (hsPkgs."random" or (buildDepError "random"))
            (hsPkgs."safe-exceptions" or (buildDepError "safe-exceptions"))
            (hsPkgs."serokell-util" or (buildDepError "serokell-util"))
            (hsPkgs."text" or (buildDepError "text"))
            ];
          buildable = if flags.benchmarks then true else false;
          };
        "bench-log-reader" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."attoparsec" or (buildDepError "attoparsec"))
            (hsPkgs."cardano-sl-util" or (buildDepError "cardano-sl-util"))
            (hsPkgs."conduit" or (buildDepError "conduit"))
            (hsPkgs."conduit-extra" or (buildDepError "conduit-extra"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."lens" or (buildDepError "lens"))
            (hsPkgs."mtl" or (buildDepError "mtl"))
            (hsPkgs."optparse-simple" or (buildDepError "optparse-simple"))
            (hsPkgs."resourcet" or (buildDepError "resourcet"))
            (hsPkgs."safe-exceptions" or (buildDepError "safe-exceptions"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."formatting" or (buildDepError "formatting"))
            (hsPkgs."unliftio-core" or (buildDepError "unliftio-core"))
            ];
          buildable = if flags.benchmarks then true else false;
          };
        };
      tests = {
        "cardano-sl-networking-test" = {
          depends = [
            (hsPkgs."async" or (buildDepError "async"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."binary" or (buildDepError "binary"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."cardano-sl-networking" or (buildDepError "cardano-sl-networking"))
            (hsPkgs."cardano-sl-util" or (buildDepError "cardano-sl-util"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."hspec" or (buildDepError "hspec"))
            (hsPkgs."hspec-core" or (buildDepError "hspec-core"))
            (hsPkgs."lens" or (buildDepError "lens"))
            (hsPkgs."mtl" or (buildDepError "mtl"))
            (hsPkgs."network-transport" or (buildDepError "network-transport"))
            (hsPkgs."network-transport-tcp" or (buildDepError "network-transport-tcp"))
            (hsPkgs."network-transport-inmemory" or (buildDepError "network-transport-inmemory"))
            (hsPkgs."QuickCheck" or (buildDepError "QuickCheck"))
            (hsPkgs."random" or (buildDepError "random"))
            (hsPkgs."serokell-util" or (buildDepError "serokell-util"))
            (hsPkgs."stm" or (buildDepError "stm"))
            (hsPkgs."time-units" or (buildDepError "time-units"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover or (pkgs.buildPackages.hspec-discover or (buildToolDepError "hspec-discover")))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "qdisc-simulation" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."async" or (buildDepError "async"))
            (hsPkgs."network-transport-tcp" or (buildDepError "network-transport-tcp"))
            (hsPkgs."network-transport" or (buildDepError "network-transport"))
            (hsPkgs."time-units" or (buildDepError "time-units"))
            (hsPkgs."stm" or (buildDepError "stm"))
            (hsPkgs."mwc-random" or (buildDepError "mwc-random"))
            (hsPkgs."statistics" or (buildDepError "statistics"))
            (hsPkgs."vector" or (buildDepError "vector"))
            (hsPkgs."time" or (buildDepError "time"))
            ];
          buildable = true;
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././networking; }