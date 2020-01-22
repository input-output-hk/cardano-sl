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
      identifier = { name = "cardano-wallet"; version = "3.2.0"; };
      license = "Apache-2.0";
      copyright = "2018 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK Engineering Team";
      homepage = "https://github.com/input-output-hk/cardano-wallet";
      url = "";
      synopsis = "The Wallet Backend for a Cardano node.";
      description = "Please see README.md";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."acid-state" or (buildDepError "acid-state"))
          (hsPkgs."aeson" or (buildDepError "aeson"))
          (hsPkgs."aeson-options" or (buildDepError "aeson-options"))
          (hsPkgs."aeson-pretty" or (buildDepError "aeson-pretty"))
          (hsPkgs."async" or (buildDepError "async"))
          (hsPkgs."base58-bytestring" or (buildDepError "base58-bytestring"))
          (hsPkgs."beam-core" or (buildDepError "beam-core"))
          (hsPkgs."beam-migrate" or (buildDepError "beam-migrate"))
          (hsPkgs."beam-sqlite" or (buildDepError "beam-sqlite"))
          (hsPkgs."bifunctors" or (buildDepError "bifunctors"))
          (hsPkgs."binary" or (buildDepError "binary"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."cardano-crypto" or (buildDepError "cardano-crypto"))
          (hsPkgs."cardano-sl" or (buildDepError "cardano-sl"))
          (hsPkgs."cardano-sl-binary" or (buildDepError "cardano-sl-binary"))
          (hsPkgs."cardano-sl-chain" or (buildDepError "cardano-sl-chain"))
          (hsPkgs."cardano-sl-client" or (buildDepError "cardano-sl-client"))
          (hsPkgs."cardano-sl-core" or (buildDepError "cardano-sl-core"))
          (hsPkgs."cardano-sl-core-test" or (buildDepError "cardano-sl-core-test"))
          (hsPkgs."cardano-sl-crypto" or (buildDepError "cardano-sl-crypto"))
          (hsPkgs."cardano-sl-db" or (buildDepError "cardano-sl-db"))
          (hsPkgs."cardano-sl-mnemonic" or (buildDepError "cardano-sl-mnemonic"))
          (hsPkgs."cardano-sl-infra" or (buildDepError "cardano-sl-infra"))
          (hsPkgs."cardano-sl-networking" or (buildDepError "cardano-sl-networking"))
          (hsPkgs."cardano-sl-node" or (buildDepError "cardano-sl-node"))
          (hsPkgs."cardano-sl-node-ipc" or (buildDepError "cardano-sl-node-ipc"))
          (hsPkgs."cardano-sl-util" or (buildDepError "cardano-sl-util"))
          (hsPkgs."cardano-sl-utxo" or (buildDepError "cardano-sl-utxo"))
          (hsPkgs."cardano-sl-x509" or (buildDepError "cardano-sl-x509"))
          (hsPkgs."cereal" or (buildDepError "cereal"))
          (hsPkgs."clock" or (buildDepError "clock"))
          (hsPkgs."conduit" or (buildDepError "conduit"))
          (hsPkgs."containers" or (buildDepError "containers"))
          (hsPkgs."cryptonite" or (buildDepError "cryptonite"))
          (hsPkgs."data-default" or (buildDepError "data-default"))
          (hsPkgs."data-default-class" or (buildDepError "data-default-class"))
          (hsPkgs."directory" or (buildDepError "directory"))
          (hsPkgs."exceptions" or (buildDepError "exceptions"))
          (hsPkgs."filepath" or (buildDepError "filepath"))
          (hsPkgs."foldl" or (buildDepError "foldl"))
          (hsPkgs."formatting" or (buildDepError "formatting"))
          (hsPkgs."formatting" or (buildDepError "formatting"))
          (hsPkgs."generics-sop" or (buildDepError "generics-sop"))
          (hsPkgs."http-api-data" or (buildDepError "http-api-data"))
          (hsPkgs."http-client" or (buildDepError "http-client"))
          (hsPkgs."http-types" or (buildDepError "http-types"))
          (hsPkgs."ixset-typed" or (buildDepError "ixset-typed"))
          (hsPkgs."lens" or (buildDepError "lens"))
          (hsPkgs."memory" or (buildDepError "memory"))
          (hsPkgs."mtl" or (buildDepError "mtl"))
          (hsPkgs."mwc-random" or (buildDepError "mwc-random"))
          (hsPkgs."neat-interpolation" or (buildDepError "neat-interpolation"))
          (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
          (hsPkgs."QuickCheck" or (buildDepError "QuickCheck"))
          (hsPkgs."reflection" or (buildDepError "reflection"))
          (hsPkgs."resourcet" or (buildDepError "resourcet"))
          (hsPkgs."retry" or (buildDepError "retry"))
          (hsPkgs."safe-exceptions" or (buildDepError "safe-exceptions"))
          (hsPkgs."safecopy" or (buildDepError "safecopy"))
          (hsPkgs."serokell-util" or (buildDepError "serokell-util"))
          (hsPkgs."servant" or (buildDepError "servant"))
          (hsPkgs."servant-client" or (buildDepError "servant-client"))
          (hsPkgs."servant-client-core" or (buildDepError "servant-client-core"))
          (hsPkgs."servant-server" or (buildDepError "servant-server"))
          (hsPkgs."servant-swagger" or (buildDepError "servant-swagger"))
          (hsPkgs."servant-swagger-ui" or (buildDepError "servant-swagger-ui"))
          (hsPkgs."servant-swagger-ui-core" or (buildDepError "servant-swagger-ui-core"))
          (hsPkgs."servant-swagger-ui-redoc" or (buildDepError "servant-swagger-ui-redoc"))
          (hsPkgs."sqlite-simple" or (buildDepError "sqlite-simple"))
          (hsPkgs."sqlite-simple-errors" or (buildDepError "sqlite-simple-errors"))
          (hsPkgs."stm" or (buildDepError "stm"))
          (hsPkgs."stm-chans" or (buildDepError "stm-chans"))
          (hsPkgs."strict" or (buildDepError "strict"))
          (hsPkgs."strict-concurrency" or (buildDepError "strict-concurrency"))
          (hsPkgs."swagger2" or (buildDepError "swagger2"))
          (hsPkgs."tar" or (buildDepError "tar"))
          (hsPkgs."text" or (buildDepError "text"))
          (hsPkgs."time" or (buildDepError "time"))
          (hsPkgs."time-units" or (buildDepError "time-units"))
          (hsPkgs."tls" or (buildDepError "tls"))
          (hsPkgs."transformers" or (buildDepError "transformers"))
          (hsPkgs."universum" or (buildDepError "universum"))
          (hsPkgs."unliftio-core" or (buildDepError "unliftio-core"))
          (hsPkgs."unordered-containers" or (buildDepError "unordered-containers"))
          (hsPkgs."uuid" or (buildDepError "uuid"))
          (hsPkgs."vector" or (buildDepError "vector"))
          (hsPkgs."wai" or (buildDepError "wai"))
          (hsPkgs."wai-middleware-throttle" or (buildDepError "wai-middleware-throttle"))
          (hsPkgs."warp" or (buildDepError "warp"))
          (hsPkgs."x509" or (buildDepError "x509"))
          (hsPkgs."zlib" or (buildDepError "zlib"))
          ];
        buildable = true;
        };
      exes = {
        "cardano-node" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."cardano-sl" or (buildDepError "cardano-sl"))
            (hsPkgs."cardano-sl-util" or (buildDepError "cardano-sl-util"))
            (hsPkgs."cardano-wallet" or (buildDepError "cardano-wallet"))
            (hsPkgs."universum" or (buildDepError "universum"))
            ];
          buildable = true;
          };
        "cardano-generate-swagger-file" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."aeson" or (buildDepError "aeson"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."cardano-sl-chain" or (buildDepError "cardano-sl-chain"))
            (hsPkgs."cardano-sl-util" or (buildDepError "cardano-sl-util"))
            (hsPkgs."cardano-wallet" or (buildDepError "cardano-wallet"))
            (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
            (hsPkgs."swagger2" or (buildDepError "swagger2"))
            (hsPkgs."universum" or (buildDepError "universum"))
            ];
          buildable = true;
          };
        };
      tests = {
        "unit" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."acid-state" or (buildDepError "acid-state"))
            (hsPkgs."aeson" or (buildDepError "aeson"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."cardano-crypto" or (buildDepError "cardano-crypto"))
            (hsPkgs."cardano-sl" or (buildDepError "cardano-sl"))
            (hsPkgs."cardano-sl-binary" or (buildDepError "cardano-sl-binary"))
            (hsPkgs."cardano-sl-chain" or (buildDepError "cardano-sl-chain"))
            (hsPkgs."cardano-sl-chain-test" or (buildDepError "cardano-sl-chain-test"))
            (hsPkgs."cardano-sl-client" or (buildDepError "cardano-sl-client"))
            (hsPkgs."cardano-sl-core" or (buildDepError "cardano-sl-core"))
            (hsPkgs."cardano-sl-core-test" or (buildDepError "cardano-sl-core-test"))
            (hsPkgs."cardano-sl-crypto" or (buildDepError "cardano-sl-crypto"))
            (hsPkgs."cardano-sl-db" or (buildDepError "cardano-sl-db"))
            (hsPkgs."cardano-sl-infra" or (buildDepError "cardano-sl-infra"))
            (hsPkgs."cardano-sl-mnemonic" or (buildDepError "cardano-sl-mnemonic"))
            (hsPkgs."cardano-sl-util" or (buildDepError "cardano-sl-util"))
            (hsPkgs."cardano-sl-util-test" or (buildDepError "cardano-sl-util-test"))
            (hsPkgs."cardano-sl-utxo" or (buildDepError "cardano-sl-utxo"))
            (hsPkgs."cardano-wallet" or (buildDepError "cardano-wallet"))
            (hsPkgs."cereal" or (buildDepError "cereal"))
            (hsPkgs."conduit" or (buildDepError "conduit"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."cryptonite" or (buildDepError "cryptonite"))
            (hsPkgs."data-default" or (buildDepError "data-default"))
            (hsPkgs."directory" or (buildDepError "directory"))
            (hsPkgs."formatting" or (buildDepError "formatting"))
            (hsPkgs."hedgehog" or (buildDepError "hedgehog"))
            (hsPkgs."hspec" or (buildDepError "hspec"))
            (hsPkgs."hspec-core" or (buildDepError "hspec-core"))
            (hsPkgs."insert-ordered-containers" or (buildDepError "insert-ordered-containers"))
            (hsPkgs."lens" or (buildDepError "lens"))
            (hsPkgs."mtl" or (buildDepError "mtl"))
            (hsPkgs."normaldistribution" or (buildDepError "normaldistribution"))
            (hsPkgs."QuickCheck" or (buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-instances" or (buildDepError "quickcheck-instances"))
            (hsPkgs."random" or (buildDepError "random"))
            (hsPkgs."safe-exceptions" or (buildDepError "safe-exceptions"))
            (hsPkgs."safecopy" or (buildDepError "safecopy"))
            (hsPkgs."serokell-util" or (buildDepError "serokell-util"))
            (hsPkgs."servant" or (buildDepError "servant"))
            (hsPkgs."servant-server" or (buildDepError "servant-server"))
            (hsPkgs."servant-swagger" or (buildDepError "servant-swagger"))
            (hsPkgs."string-conv" or (buildDepError "string-conv"))
            (hsPkgs."swagger2" or (buildDepError "swagger2"))
            (hsPkgs."tabl" or (buildDepError "tabl"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."time" or (buildDepError "time"))
            (hsPkgs."time-units" or (buildDepError "time-units"))
            (hsPkgs."universum" or (buildDepError "universum"))
            (hsPkgs."vector" or (buildDepError "vector"))
            ];
          buildable = true;
          };
        "nightly" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."async" or (buildDepError "async"))
            (hsPkgs."cardano-sl-chain" or (buildDepError "cardano-sl-chain"))
            (hsPkgs."cardano-sl-core" or (buildDepError "cardano-sl-core"))
            (hsPkgs."cardano-wallet" or (buildDepError "cardano-wallet"))
            (hsPkgs."formatting" or (buildDepError "formatting"))
            (hsPkgs."hspec" or (buildDepError "hspec"))
            (hsPkgs."hspec-core" or (buildDepError "hspec-core"))
            (hsPkgs."QuickCheck" or (buildDepError "QuickCheck"))
            (hsPkgs."safe-exceptions" or (buildDepError "safe-exceptions"))
            (hsPkgs."serokell-util" or (buildDepError "serokell-util"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."universum" or (buildDepError "universum"))
            ];
          buildable = true;
          };
        "integration" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."QuickCheck" or (buildDepError "QuickCheck"))
            (hsPkgs."servant-client" or (buildDepError "servant-client"))
            (hsPkgs."aeson" or (buildDepError "aeson"))
            (hsPkgs."aeson-qq" or (buildDepError "aeson-qq"))
            (hsPkgs."async" or (buildDepError "async"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."cardano-sl" or (buildDepError "cardano-sl"))
            (hsPkgs."cardano-sl-chain" or (buildDepError "cardano-sl-chain"))
            (hsPkgs."cardano-sl-client" or (buildDepError "cardano-sl-client"))
            (hsPkgs."cardano-sl-cluster" or (buildDepError "cardano-sl-cluster"))
            (hsPkgs."cardano-sl-core" or (buildDepError "cardano-sl-core"))
            (hsPkgs."cardano-sl-crypto" or (buildDepError "cardano-sl-crypto"))
            (hsPkgs."cardano-sl-mnemonic" or (buildDepError "cardano-sl-mnemonic"))
            (hsPkgs."cardano-sl-util" or (buildDepError "cardano-sl-util"))
            (hsPkgs."cardano-wallet" or (buildDepError "cardano-wallet"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."cryptonite" or (buildDepError "cryptonite"))
            (hsPkgs."filepath" or (buildDepError "filepath"))
            (hsPkgs."generic-lens" or (buildDepError "generic-lens"))
            (hsPkgs."hspec" or (buildDepError "hspec"))
            (hsPkgs."hspec-core" or (buildDepError "hspec-core"))
            (hsPkgs."hspec-expectations-lifted" or (buildDepError "hspec-expectations-lifted"))
            (hsPkgs."http-api-data" or (buildDepError "http-api-data"))
            (hsPkgs."http-client" or (buildDepError "http-client"))
            (hsPkgs."http-types" or (buildDepError "http-types"))
            (hsPkgs."memory" or (buildDepError "memory"))
            (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
            (hsPkgs."servant-client-core" or (buildDepError "servant-client-core"))
            (hsPkgs."template-haskell" or (buildDepError "template-haskell"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."universum" or (buildDepError "universum"))
            ];
          buildable = true;
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././wallet; }