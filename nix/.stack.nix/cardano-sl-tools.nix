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
    flags = { for-installer = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "cardano-sl-tools"; version = "3.2.0"; };
      license = "Apache-2.0";
      copyright = "2016 IOHK";
      maintainer = "hi@serokell.io";
      author = "Serokell";
      homepage = "";
      url = "";
      synopsis = "Cardano SL - Tools";
      description = "Cardano SL - Tools";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."aeson" or (buildDepError "aeson"))
          (hsPkgs."containers" or (buildDepError "containers"))
          (hsPkgs."data-default" or (buildDepError "data-default"))
          (hsPkgs."directory" or (buildDepError "directory"))
          (hsPkgs."filepath" or (buildDepError "filepath"))
          (hsPkgs."network-transport-tcp" or (buildDepError "network-transport-tcp"))
          (hsPkgs."parsers" or (buildDepError "parsers"))
          (hsPkgs."text" or (buildDepError "text"))
          (hsPkgs."trifecta" or (buildDepError "trifecta"))
          (hsPkgs."universum" or (buildDepError "universum"))
          ];
        buildable = true;
        };
      exes = {
        "cardano-genupdate" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."ansi-wl-pprint" or (buildDepError "ansi-wl-pprint"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."cardano-sl" or (buildDepError "cardano-sl"))
            (hsPkgs."cardano-sl-util" or (buildDepError "cardano-sl-util"))
            (hsPkgs."cryptonite" or (buildDepError "cryptonite"))
            (hsPkgs."neat-interpolation" or (buildDepError "neat-interpolation"))
            (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
            (hsPkgs."filepath" or (buildDepError "filepath"))
            (hsPkgs."formatting" or (buildDepError "formatting"))
            (hsPkgs."process" or (buildDepError "process"))
            (hsPkgs."tar" or (buildDepError "tar"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."universum" or (buildDepError "universum"))
            (hsPkgs."unix-compat" or (buildDepError "unix-compat"))
            ];
          buildable = true;
          };
        "cardano-keygen" = {
          depends = (pkgs.lib).optionals (!flags.for-installer) [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."base58-bytestring" or (buildDepError "base58-bytestring"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."canonical-json" or (buildDepError "canonical-json"))
            (hsPkgs."cardano-sl" or (buildDepError "cardano-sl"))
            (hsPkgs."cardano-sl-chain" or (buildDepError "cardano-sl-chain"))
            (hsPkgs."cardano-sl-core" or (buildDepError "cardano-sl-core"))
            (hsPkgs."cardano-sl-crypto" or (buildDepError "cardano-sl-crypto"))
            (hsPkgs."cardano-sl-util" or (buildDepError "cardano-sl-util"))
            (hsPkgs."directory" or (buildDepError "directory"))
            (hsPkgs."filepath" or (buildDepError "filepath"))
            (hsPkgs."formatting" or (buildDepError "formatting"))
            (hsPkgs."Glob" or (buildDepError "Glob"))
            (hsPkgs."lens" or (buildDepError "lens"))
            (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
            (hsPkgs."serokell-util" or (buildDepError "serokell-util"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."universum" or (buildDepError "universum"))
            ];
          buildable = if flags.for-installer then false else true;
          };
        "cardano-launcher" = {
          depends = [
            (hsPkgs."aeson" or (buildDepError "aeson"))
            (hsPkgs."aeson-options" or (buildDepError "aeson-options"))
            (hsPkgs."ansi-wl-pprint" or (buildDepError "ansi-wl-pprint"))
            (hsPkgs."async" or (buildDepError "async"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."cardano-sl" or (buildDepError "cardano-sl"))
            (hsPkgs."cardano-sl-chain" or (buildDepError "cardano-sl-chain"))
            (hsPkgs."cardano-sl-core" or (buildDepError "cardano-sl-core"))
            (hsPkgs."cardano-sl-crypto" or (buildDepError "cardano-sl-crypto"))
            (hsPkgs."cardano-sl-db" or (buildDepError "cardano-sl-db"))
            (hsPkgs."cardano-sl-infra" or (buildDepError "cardano-sl-infra"))
            (hsPkgs."cardano-sl-tools" or (buildDepError "cardano-sl-tools"))
            (hsPkgs."cardano-sl-util" or (buildDepError "cardano-sl-util"))
            (hsPkgs."directory" or (buildDepError "directory"))
            (hsPkgs."filepath" or (buildDepError "filepath"))
            (hsPkgs."formatting" or (buildDepError "formatting"))
            (hsPkgs."lens" or (buildDepError "lens"))
            (hsPkgs."lifted-async" or (buildDepError "lifted-async"))
            (hsPkgs."neat-interpolation" or (buildDepError "neat-interpolation"))
            (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
            (hsPkgs."process" or (buildDepError "process"))
            (hsPkgs."safe-exceptions" or (buildDepError "safe-exceptions"))
            (hsPkgs."silently" or (buildDepError "silently"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."time-units" or (buildDepError "time-units"))
            (hsPkgs."universum" or (buildDepError "universum"))
            (hsPkgs."unordered-containers" or (buildDepError "unordered-containers"))
            (hsPkgs."yaml" or (buildDepError "yaml"))
            ] ++ (if !system.isWindows
            then [ (hsPkgs."unix" or (buildDepError "unix")) ]
            else [ (hsPkgs."Win32" or (buildDepError "Win32")) ]);
          buildable = true;
          };
        "cardano-addr-convert" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."ansi-wl-pprint" or (buildDepError "ansi-wl-pprint"))
            (hsPkgs."cardano-sl" or (buildDepError "cardano-sl"))
            (hsPkgs."cardano-sl-core" or (buildDepError "cardano-sl-core"))
            (hsPkgs."cardano-sl-crypto" or (buildDepError "cardano-sl-crypto"))
            (hsPkgs."cardano-sl-util" or (buildDepError "cardano-sl-util"))
            (hsPkgs."neat-interpolation" or (buildDepError "neat-interpolation"))
            (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."universum" or (buildDepError "universum"))
            ];
          buildable = true;
          };
        "cardano-cli-docs" = {
          depends = (pkgs.lib).optionals (!flags.for-installer) [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."cardano-sl" or (buildDepError "cardano-sl"))
            (hsPkgs."directory" or (buildDepError "directory"))
            (hsPkgs."filepath" or (buildDepError "filepath"))
            (hsPkgs."neat-interpolation" or (buildDepError "neat-interpolation"))
            (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
            (hsPkgs."process" or (buildDepError "process"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."universum" or (buildDepError "universum"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.cpphs or (pkgs.buildPackages.cpphs or (buildToolDepError "cpphs")))
            ];
          buildable = if flags.for-installer then false else true;
          };
        "cardano-blockchain-analyser" = {
          depends = (pkgs.lib).optionals (!flags.for-installer) [
            (hsPkgs."ansi-wl-pprint" or (buildDepError "ansi-wl-pprint"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."cardano-sl" or (buildDepError "cardano-sl"))
            (hsPkgs."cardano-sl-binary" or (buildDepError "cardano-sl-binary"))
            (hsPkgs."cardano-sl-chain" or (buildDepError "cardano-sl-chain"))
            (hsPkgs."cardano-sl-core" or (buildDepError "cardano-sl-core"))
            (hsPkgs."cardano-sl-crypto" or (buildDepError "cardano-sl-crypto"))
            (hsPkgs."cardano-sl-db" or (buildDepError "cardano-sl-db"))
            (hsPkgs."cardano-sl-util" or (buildDepError "cardano-sl-util"))
            (hsPkgs."directory" or (buildDepError "directory"))
            (hsPkgs."formatting" or (buildDepError "formatting"))
            (hsPkgs."lens" or (buildDepError "lens"))
            (hsPkgs."mtl" or (buildDepError "mtl"))
            (hsPkgs."neat-interpolation" or (buildDepError "neat-interpolation"))
            (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
            (hsPkgs."serokell-util" or (buildDepError "serokell-util"))
            (hsPkgs."tabl" or (buildDepError "tabl"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."universum" or (buildDepError "universum"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.cpphs or (pkgs.buildPackages.cpphs or (buildToolDepError "cpphs")))
            ];
          buildable = if flags.for-installer then false else true;
          };
        "cardano-x509-certificates" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."cardano-sl-x509" or (buildDepError "cardano-sl-x509"))
            (hsPkgs."filepath" or (buildDepError "filepath"))
            (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
            (hsPkgs."universum" or (buildDepError "universum"))
            ];
          buildable = true;
          };
        "genesis-hash" = {
          depends = (pkgs.lib).optionals (!flags.for-installer) [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."universum" or (buildDepError "universum"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."cryptonite" or (buildDepError "cryptonite"))
            (hsPkgs."canonical-json" or (buildDepError "canonical-json"))
            ];
          buildable = if flags.for-installer then false else true;
          };
        "wallet-extractor" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."universum" or (buildDepError "universum"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."cardano-sl" or (buildDepError "cardano-sl"))
            (hsPkgs."cardano-sl-util" or (buildDepError "cardano-sl-util"))
            ];
          buildable = true;
          };
        };
      tests = {
        "cardano-sl-tools-test" = {
          depends = [
            (hsPkgs."aeson" or (buildDepError "aeson"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."cardano-sl-tools" or (buildDepError "cardano-sl-tools"))
            (hsPkgs."cardano-sl-util-test" or (buildDepError "cardano-sl-util-test"))
            (hsPkgs."directory" or (buildDepError "directory"))
            (hsPkgs."hspec" or (buildDepError "hspec"))
            (hsPkgs."temporary" or (buildDepError "temporary"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.cpphs or (pkgs.buildPackages.cpphs or (buildToolDepError "cpphs")))
            ];
          buildable = true;
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././tools; }