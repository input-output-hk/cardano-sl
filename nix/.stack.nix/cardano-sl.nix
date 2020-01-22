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
      identifier = { name = "cardano-sl"; version = "3.2.0"; };
      license = "Apache-2.0";
      copyright = "2016 IOHK";
      maintainer = "Serokell <hi@serokell.io>";
      author = "Serokell";
      homepage = "";
      url = "";
      synopsis = "Cardano SL main implementation";
      description = "Please see README.md";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."base64-bytestring" or (buildDepError "base64-bytestring"))
          (hsPkgs."aeson" or (buildDepError "aeson"))
          (hsPkgs."aeson-options" or (buildDepError "aeson-options"))
          (hsPkgs."ansi-terminal" or (buildDepError "ansi-terminal"))
          (hsPkgs."ansi-wl-pprint" or (buildDepError "ansi-wl-pprint"))
          (hsPkgs."async" or (buildDepError "async"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."canonical-json" or (buildDepError "canonical-json"))
          (hsPkgs."cardano-sl-binary" or (buildDepError "cardano-sl-binary"))
          (hsPkgs."cardano-sl-binary-test" or (buildDepError "cardano-sl-binary-test"))
          (hsPkgs."cardano-sl-chain" or (buildDepError "cardano-sl-chain"))
          (hsPkgs."cardano-sl-chain-test" or (buildDepError "cardano-sl-chain-test"))
          (hsPkgs."cardano-sl-core" or (buildDepError "cardano-sl-core"))
          (hsPkgs."cardano-sl-core-test" or (buildDepError "cardano-sl-core-test"))
          (hsPkgs."cardano-sl-crypto" or (buildDepError "cardano-sl-crypto"))
          (hsPkgs."cardano-sl-crypto-test" or (buildDepError "cardano-sl-crypto-test"))
          (hsPkgs."cardano-sl-db" or (buildDepError "cardano-sl-db"))
          (hsPkgs."cardano-sl-infra" or (buildDepError "cardano-sl-infra"))
          (hsPkgs."cardano-sl-networking" or (buildDepError "cardano-sl-networking"))
          (hsPkgs."cardano-sl-util" or (buildDepError "cardano-sl-util"))
          (hsPkgs."conduit" or (buildDepError "conduit"))
          (hsPkgs."constraints" or (buildDepError "constraints"))
          (hsPkgs."containers" or (buildDepError "containers"))
          (hsPkgs."contravariant" or (buildDepError "contravariant"))
          (hsPkgs."cryptonite" or (buildDepError "cryptonite"))
          (hsPkgs."data-default" or (buildDepError "data-default"))
          (hsPkgs."directory" or (buildDepError "directory"))
          (hsPkgs."ekg-core" or (buildDepError "ekg-core"))
          (hsPkgs."ekg" or (buildDepError "ekg"))
          (hsPkgs."ether" or (buildDepError "ether"))
          (hsPkgs."exceptions" or (buildDepError "exceptions"))
          (hsPkgs."filelock" or (buildDepError "filelock"))
          (hsPkgs."filepath" or (buildDepError "filepath"))
          (hsPkgs."formatting" or (buildDepError "formatting"))
          (hsPkgs."formatting" or (buildDepError "formatting"))
          (hsPkgs."generics-sop" or (buildDepError "generics-sop"))
          (hsPkgs."hspec" or (buildDepError "hspec"))
          (hsPkgs."http-api-data" or (buildDepError "http-api-data"))
          (hsPkgs."http-client" or (buildDepError "http-client"))
          (hsPkgs."http-client-tls" or (buildDepError "http-client-tls"))
          (hsPkgs."http-conduit" or (buildDepError "http-conduit"))
          (hsPkgs."http-types" or (buildDepError "http-types"))
          (hsPkgs."lens" or (buildDepError "lens"))
          (hsPkgs."lifted-async" or (buildDepError "lifted-async"))
          (hsPkgs."memory" or (buildDepError "memory"))
          (hsPkgs."monad-control" or (buildDepError "monad-control"))
          (hsPkgs."mtl" or (buildDepError "mtl"))
          (hsPkgs."neat-interpolation" or (buildDepError "neat-interpolation"))
          (hsPkgs."network" or (buildDepError "network"))
          (hsPkgs."network-transport" or (buildDepError "network-transport"))
          (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
          (hsPkgs."megaparsec" or (buildDepError "megaparsec"))
          (hsPkgs."pvss" or (buildDepError "pvss"))
          (hsPkgs."QuickCheck" or (buildDepError "QuickCheck"))
          (hsPkgs."quickcheck-instances" or (buildDepError "quickcheck-instances"))
          (hsPkgs."random" or (buildDepError "random"))
          (hsPkgs."reflection" or (buildDepError "reflection"))
          (hsPkgs."safe-exceptions" or (buildDepError "safe-exceptions"))
          (hsPkgs."serokell-util" or (buildDepError "serokell-util"))
          (hsPkgs."servant" or (buildDepError "servant"))
          (hsPkgs."servant-client" or (buildDepError "servant-client"))
          (hsPkgs."servant-client-core" or (buildDepError "servant-client-core"))
          (hsPkgs."servant-server" or (buildDepError "servant-server"))
          (hsPkgs."servant-swagger" or (buildDepError "servant-swagger"))
          (hsPkgs."servant-swagger-ui" or (buildDepError "servant-swagger-ui"))
          (hsPkgs."servant-swagger-ui-core" or (buildDepError "servant-swagger-ui-core"))
          (hsPkgs."servant-swagger-ui-redoc" or (buildDepError "servant-swagger-ui-redoc"))
          (hsPkgs."stm" or (buildDepError "stm"))
          (hsPkgs."streaming-commons" or (buildDepError "streaming-commons"))
          (hsPkgs."swagger2" or (buildDepError "swagger2"))
          (hsPkgs."tagged" or (buildDepError "tagged"))
          (hsPkgs."template-haskell" or (buildDepError "template-haskell"))
          (hsPkgs."text" or (buildDepError "text"))
          (hsPkgs."time" or (buildDepError "time"))
          (hsPkgs."time-units" or (buildDepError "time-units"))
          (hsPkgs."tls" or (buildDepError "tls"))
          (hsPkgs."transformers" or (buildDepError "transformers"))
          (hsPkgs."universum" or (buildDepError "universum"))
          (hsPkgs."unliftio" or (buildDepError "unliftio"))
          (hsPkgs."unordered-containers" or (buildDepError "unordered-containers"))
          (hsPkgs."wai" or (buildDepError "wai"))
          (hsPkgs."warp" or (buildDepError "warp"))
          (hsPkgs."warp-tls" or (buildDepError "warp-tls"))
          (hsPkgs."x509" or (buildDepError "x509"))
          (hsPkgs."x509-store" or (buildDepError "x509-store"))
          (hsPkgs."x509-validation" or (buildDepError "x509-validation"))
          (hsPkgs."yaml" or (buildDepError "yaml"))
          (hsPkgs."cborg" or (buildDepError "cborg"))
          ] ++ (pkgs.lib).optional (!system.isWindows) (hsPkgs."unix" or (buildDepError "unix"))) ++ (pkgs.lib).optional (!system.isWindows && !system.isFreebsd) (hsPkgs."systemd" or (buildDepError "systemd"));
        build-tools = [
          (hsPkgs.buildPackages.cpphs or (pkgs.buildPackages.cpphs or (buildToolDepError "cpphs")))
          ];
        buildable = true;
        };
      tests = {
        "cardano-test" = {
          depends = [
            (hsPkgs."QuickCheck" or (buildDepError "QuickCheck"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."cardano-crypto" or (buildDepError "cardano-crypto"))
            (hsPkgs."cardano-sl" or (buildDepError "cardano-sl"))
            (hsPkgs."cardano-sl-binary" or (buildDepError "cardano-sl-binary"))
            (hsPkgs."cardano-sl-binary-test" or (buildDepError "cardano-sl-binary-test"))
            (hsPkgs."cardano-sl-chain" or (buildDepError "cardano-sl-chain"))
            (hsPkgs."cardano-sl-chain-test" or (buildDepError "cardano-sl-chain-test"))
            (hsPkgs."cardano-sl-core" or (buildDepError "cardano-sl-core"))
            (hsPkgs."cardano-sl-core-test" or (buildDepError "cardano-sl-core-test"))
            (hsPkgs."cardano-sl-crypto" or (buildDepError "cardano-sl-crypto"))
            (hsPkgs."cardano-sl-crypto-test" or (buildDepError "cardano-sl-crypto-test"))
            (hsPkgs."cardano-sl-db" or (buildDepError "cardano-sl-db"))
            (hsPkgs."cardano-sl-db-test" or (buildDepError "cardano-sl-db-test"))
            (hsPkgs."cardano-sl-infra" or (buildDepError "cardano-sl-infra"))
            (hsPkgs."cardano-sl-infra-test" or (buildDepError "cardano-sl-infra-test"))
            (hsPkgs."cardano-sl-networking" or (buildDepError "cardano-sl-networking"))
            (hsPkgs."cardano-sl-util" or (buildDepError "cardano-sl-util"))
            (hsPkgs."cardano-sl-util-test" or (buildDepError "cardano-sl-util-test"))
            (hsPkgs."conduit" or (buildDepError "conduit"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."cryptonite" or (buildDepError "cryptonite"))
            (hsPkgs."data-default" or (buildDepError "data-default"))
            (hsPkgs."deepseq" or (buildDepError "deepseq"))
            (hsPkgs."extra" or (buildDepError "extra"))
            (hsPkgs."filelock" or (buildDepError "filelock"))
            (hsPkgs."formatting" or (buildDepError "formatting"))
            (hsPkgs."generic-arbitrary" or (buildDepError "generic-arbitrary"))
            (hsPkgs."hedgehog" or (buildDepError "hedgehog"))
            (hsPkgs."hspec" or (buildDepError "hspec"))
            (hsPkgs."lens" or (buildDepError "lens"))
            (hsPkgs."network-transport" or (buildDepError "network-transport"))
            (hsPkgs."network-transport-inmemory" or (buildDepError "network-transport-inmemory"))
            (hsPkgs."pvss" or (buildDepError "pvss"))
            (hsPkgs."random" or (buildDepError "random"))
            (hsPkgs."reflection" or (buildDepError "reflection"))
            (hsPkgs."serokell-util" or (buildDepError "serokell-util"))
            (hsPkgs."tagged" or (buildDepError "tagged"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."time" or (buildDepError "time"))
            (hsPkgs."time-units" or (buildDepError "time-units"))
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
        "cardano-bench-criterion" = {
          depends = [
            (hsPkgs."QuickCheck" or (buildDepError "QuickCheck"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."cardano-sl" or (buildDepError "cardano-sl"))
            (hsPkgs."cardano-sl-chain" or (buildDepError "cardano-sl-chain"))
            (hsPkgs."cardano-sl-chain-test" or (buildDepError "cardano-sl-chain-test"))
            (hsPkgs."cardano-sl-core" or (buildDepError "cardano-sl-core"))
            (hsPkgs."cardano-sl-core-test" or (buildDepError "cardano-sl-core-test"))
            (hsPkgs."cardano-sl-crypto" or (buildDepError "cardano-sl-crypto"))
            (hsPkgs."cardano-sl-crypto-test" or (buildDepError "cardano-sl-crypto-test"))
            (hsPkgs."cardano-sl-db" or (buildDepError "cardano-sl-db"))
            (hsPkgs."cardano-sl-infra" or (buildDepError "cardano-sl-infra"))
            (hsPkgs."cardano-sl-networking" or (buildDepError "cardano-sl-networking"))
            (hsPkgs."cardano-sl-util" or (buildDepError "cardano-sl-util"))
            (hsPkgs."cardano-sl-util-test" or (buildDepError "cardano-sl-util-test"))
            (hsPkgs."conduit" or (buildDepError "conduit"))
            (hsPkgs."criterion" or (buildDepError "criterion"))
            (hsPkgs."deepseq" or (buildDepError "deepseq"))
            (hsPkgs."formatting" or (buildDepError "formatting"))
            (hsPkgs."network-transport" or (buildDepError "network-transport"))
            (hsPkgs."network-transport-inmemory" or (buildDepError "network-transport-inmemory"))
            (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
            (hsPkgs."universum" or (buildDepError "universum"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.cpphs or (pkgs.buildPackages.cpphs or (buildToolDepError "cpphs")))
            ];
          buildable = true;
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././lib; }