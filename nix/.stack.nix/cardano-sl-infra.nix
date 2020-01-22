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
      identifier = { name = "cardano-sl-infra"; version = "3.2.0"; };
      license = "Apache-2.0";
      copyright = "2016 IOHK";
      maintainer = "hi@serokell.io";
      author = "Serokell";
      homepage = "";
      url = "";
      synopsis = "Cardano SL - infrastructural";
      description = "Cardano SL - infrastructural";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (buildDepError "aeson"))
          (hsPkgs."async" or (buildDepError "async"))
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."base64-bytestring" or (buildDepError "base64-bytestring"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."cardano-sl-binary" or (buildDepError "cardano-sl-binary"))
          (hsPkgs."cardano-sl-chain" or (buildDepError "cardano-sl-chain"))
          (hsPkgs."cardano-sl-core" or (buildDepError "cardano-sl-core"))
          (hsPkgs."cardano-sl-crypto" or (buildDepError "cardano-sl-crypto"))
          (hsPkgs."cardano-sl-db" or (buildDepError "cardano-sl-db"))
          (hsPkgs."cardano-sl-networking" or (buildDepError "cardano-sl-networking"))
          (hsPkgs."cardano-sl-util" or (buildDepError "cardano-sl-util"))
          (hsPkgs."clock" or (buildDepError "clock"))
          (hsPkgs."conduit" or (buildDepError "conduit"))
          (hsPkgs."containers" or (buildDepError "containers"))
          (hsPkgs."directory" or (buildDepError "directory"))
          (hsPkgs."dns" or (buildDepError "dns"))
          (hsPkgs."ekg-core" or (buildDepError "ekg-core"))
          (hsPkgs."ekg-statsd" or (buildDepError "ekg-statsd"))
          (hsPkgs."ekg-wai" or (buildDepError "ekg-wai"))
          (hsPkgs."ether" or (buildDepError "ether"))
          (hsPkgs."exceptions" or (buildDepError "exceptions"))
          (hsPkgs."filepath" or (buildDepError "filepath"))
          (hsPkgs."formatting" or (buildDepError "formatting"))
          (hsPkgs."hashable" or (buildDepError "hashable"))
          (hsPkgs."http-client" or (buildDepError "http-client"))
          (hsPkgs."http-client-tls" or (buildDepError "http-client-tls"))
          (hsPkgs."iproute" or (buildDepError "iproute"))
          (hsPkgs."kademlia" or (buildDepError "kademlia"))
          (hsPkgs."lens" or (buildDepError "lens"))
          (hsPkgs."megaparsec" or (buildDepError "megaparsec"))
          (hsPkgs."mtl" or (buildDepError "mtl"))
          (hsPkgs."network-info" or (buildDepError "network-info"))
          (hsPkgs."network-transport" or (buildDepError "network-transport"))
          (hsPkgs."network-transport-tcp" or (buildDepError "network-transport-tcp"))
          (hsPkgs."lzma-conduit" or (buildDepError "lzma-conduit"))
          (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
          (hsPkgs."safe-exceptions" or (buildDepError "safe-exceptions"))
          (hsPkgs."serokell-util" or (buildDepError "serokell-util"))
          (hsPkgs."stm" or (buildDepError "stm"))
          (hsPkgs."tar" or (buildDepError "tar"))
          (hsPkgs."time" or (buildDepError "time"))
          (hsPkgs."tagged" or (buildDepError "tagged"))
          (hsPkgs."vector" or (buildDepError "vector"))
          (hsPkgs."text" or (buildDepError "text"))
          (hsPkgs."time-units" or (buildDepError "time-units"))
          (hsPkgs."network-transport" or (buildDepError "network-transport"))
          (hsPkgs."universum" or (buildDepError "universum"))
          (hsPkgs."unliftio" or (buildDepError "unliftio"))
          (hsPkgs."unordered-containers" or (buildDepError "unordered-containers"))
          (hsPkgs."yaml" or (buildDepError "yaml"))
          ] ++ (pkgs.lib).optional (!system.isWindows) (hsPkgs."unix" or (buildDepError "unix"));
        build-tools = [
          (hsPkgs.buildPackages.cpphs or (pkgs.buildPackages.cpphs or (buildToolDepError "cpphs")))
          ];
        buildable = true;
        };
      tests = {
        "infra-test" = {
          depends = [
            (hsPkgs."QuickCheck" or (buildDepError "QuickCheck"))
            (hsPkgs."async" or (buildDepError "async"))
            (hsPkgs."aeson" or (buildDepError "aeson"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."cardano-sl-binary-test" or (buildDepError "cardano-sl-binary-test"))
            (hsPkgs."cardano-sl-chain" or (buildDepError "cardano-sl-chain"))
            (hsPkgs."cardano-sl-chain-test" or (buildDepError "cardano-sl-chain-test"))
            (hsPkgs."cardano-sl-core" or (buildDepError "cardano-sl-core"))
            (hsPkgs."cardano-sl-core-test" or (buildDepError "cardano-sl-core-test"))
            (hsPkgs."cardano-sl-crypto" or (buildDepError "cardano-sl-crypto"))
            (hsPkgs."cardano-sl-crypto-test" or (buildDepError "cardano-sl-crypto-test"))
            (hsPkgs."cardano-sl-infra" or (buildDepError "cardano-sl-infra"))
            (hsPkgs."cardano-sl-networking" or (buildDepError "cardano-sl-networking"))
            (hsPkgs."cardano-sl-util-test" or (buildDepError "cardano-sl-util-test"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."dns" or (buildDepError "dns"))
            (hsPkgs."generic-arbitrary" or (buildDepError "generic-arbitrary"))
            (hsPkgs."hedgehog" or (buildDepError "hedgehog"))
            (hsPkgs."hspec" or (buildDepError "hspec"))
            (hsPkgs."iproute" or (buildDepError "iproute"))
            (hsPkgs."kademlia" or (buildDepError "kademlia"))
            (hsPkgs."universum" or (buildDepError "universum"))
            (hsPkgs."yaml" or (buildDepError "yaml"))
            ];
          buildable = true;
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././infra; }