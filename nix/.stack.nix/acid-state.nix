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
    flags = { skip-state-machine-test = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "acid-state"; version = "0.15.0"; };
      license = "LicenseRef-PublicDomain";
      copyright = "";
      maintainer = "Lemmih <lemmih@gmail.com>";
      author = "David Himmelstrup";
      homepage = "https://github.com/acid-state/acid-state";
      url = "";
      synopsis = "Add ACID guarantees to any serializable Haskell data structure.";
      description = "Use regular Haskell data structures as your database and get stronger ACID guarantees than most RDBMS offer.";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."array" or (buildDepError "array"))
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."cereal" or (buildDepError "cereal"))
          (hsPkgs."containers" or (buildDepError "containers"))
          (hsPkgs."extensible-exceptions" or (buildDepError "extensible-exceptions"))
          (hsPkgs."safecopy" or (buildDepError "safecopy"))
          (hsPkgs."stm" or (buildDepError "stm"))
          (hsPkgs."directory" or (buildDepError "directory"))
          (hsPkgs."filelock" or (buildDepError "filelock"))
          (hsPkgs."filepath" or (buildDepError "filepath"))
          (hsPkgs."mtl" or (buildDepError "mtl"))
          (hsPkgs."network" or (buildDepError "network"))
          (hsPkgs."template-haskell" or (buildDepError "template-haskell"))
          (hsPkgs."th-expand-syns" or (buildDepError "th-expand-syns"))
          ] ++ (if system.isWindows
          then [ (hsPkgs."Win32" or (buildDepError "Win32")) ]
          else [ (hsPkgs."unix" or (buildDepError "unix")) ]);
        buildable = true;
        };
      tests = {
        "specs" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."acid-state" or (buildDepError "acid-state"))
            (hsPkgs."deepseq" or (buildDepError "deepseq"))
            (hsPkgs."hspec" or (buildDepError "hspec"))
            (hsPkgs."hspec-discover" or (buildDepError "hspec-discover"))
            (hsPkgs."mtl" or (buildDepError "mtl"))
            (hsPkgs."safecopy" or (buildDepError "safecopy"))
            (hsPkgs."template-haskell" or (buildDepError "template-haskell"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover or (pkgs.buildPackages.hspec-discover or (buildToolDepError "hspec-discover")))
            ];
          buildable = true;
          };
        "state-machine" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."acid-state" or (buildDepError "acid-state"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."deepseq" or (buildDepError "deepseq"))
            (hsPkgs."directory" or (buildDepError "directory"))
            (hsPkgs."hedgehog" or (buildDepError "hedgehog"))
            (hsPkgs."mtl" or (buildDepError "mtl"))
            (hsPkgs."safecopy" or (buildDepError "safecopy"))
            ];
          buildable = if flags.skip-state-machine-test then false else true;
          };
        "examples" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."acid-state" or (buildDepError "acid-state"))
            (hsPkgs."cereal" or (buildDepError "cereal"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."directory" or (buildDepError "directory"))
            (hsPkgs."mtl" or (buildDepError "mtl"))
            (hsPkgs."network" or (buildDepError "network"))
            (hsPkgs."safecopy" or (buildDepError "safecopy"))
            (hsPkgs."time" or (buildDepError "time"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "loading-benchmark" = {
          depends = [
            (hsPkgs."random" or (buildDepError "random"))
            (hsPkgs."directory" or (buildDepError "directory"))
            (hsPkgs."system-fileio" or (buildDepError "system-fileio"))
            (hsPkgs."system-filepath" or (buildDepError "system-filepath"))
            (hsPkgs."criterion" or (buildDepError "criterion"))
            (hsPkgs."mtl" or (buildDepError "mtl"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."acid-state" or (buildDepError "acid-state"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/parsonsmatt/acid-state";
      rev = "a1b23e2056f134e53f705a694ab85deeecabec5c";
      sha256 = "0mgdk8252g7wbb0afyn21pcn3bwh4vainy3h2d0xsv4hlpgqgnw8";
      });
    }