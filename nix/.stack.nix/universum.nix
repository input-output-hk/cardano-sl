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
      specVersion = "1.18";
      identifier = { name = "universum"; version = "1.2.0"; };
      license = "MIT";
      copyright = "2016 Stephen Diehl, 2016-2018 Serokell";
      maintainer = "Serokell <hi@serokell.io>";
      author = "Stephen Diehl, @serokell";
      homepage = "https://github.com/serokell/universum";
      url = "";
      synopsis = "Custom prelude used in Serokell";
      description = "See README.md file for more details.";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."containers" or (buildDepError "containers"))
          (hsPkgs."deepseq" or (buildDepError "deepseq"))
          (hsPkgs."ghc-prim" or (buildDepError "ghc-prim"))
          (hsPkgs."hashable" or (buildDepError "hashable"))
          (hsPkgs."microlens" or (buildDepError "microlens"))
          (hsPkgs."microlens-mtl" or (buildDepError "microlens-mtl"))
          (hsPkgs."mtl" or (buildDepError "mtl"))
          (hsPkgs."safe-exceptions" or (buildDepError "safe-exceptions"))
          (hsPkgs."stm" or (buildDepError "stm"))
          (hsPkgs."text" or (buildDepError "text"))
          (hsPkgs."transformers" or (buildDepError "transformers"))
          (hsPkgs."type-operators" or (buildDepError "type-operators"))
          (hsPkgs."unordered-containers" or (buildDepError "unordered-containers"))
          (hsPkgs."utf8-string" or (buildDepError "utf8-string"))
          (hsPkgs."vector" or (buildDepError "vector"))
          (hsPkgs."formatting" or (buildDepError "formatting"))
          (hsPkgs."fmt" or (buildDepError "fmt"))
          ];
        buildable = true;
        };
      tests = {
        "universum-test" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."universum" or (buildDepError "universum"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."utf8-string" or (buildDepError "utf8-string"))
            (hsPkgs."hedgehog" or (buildDepError "hedgehog"))
            (hsPkgs."tasty" or (buildDepError "tasty"))
            (hsPkgs."tasty-hedgehog" or (buildDepError "tasty-hedgehog"))
            ];
          buildable = true;
          };
        "universum-doctest" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."doctest" or (buildDepError "doctest"))
            (hsPkgs."Glob" or (buildDepError "Glob"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "universum-benchmark" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."universum" or (buildDepError "universum"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."gauge" or (buildDepError "gauge"))
            (hsPkgs."unordered-containers" or (buildDepError "unordered-containers"))
            ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).eq "7.10.3") (hsPkgs."semigroups" or (buildDepError "semigroups"));
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/universum";
      rev = "15f7db758ff5a24b874de2247f6f7a4576562da5";
      sha256 = "127bs29zpjcc40777dv7figk05gd00b9ja57sp11w440qr5h72hk";
      });
    }