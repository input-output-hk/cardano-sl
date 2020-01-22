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
      specVersion = "2.0";
      identifier = { name = "log-warper"; version = "1.8.10.1"; };
      license = "MIT";
      copyright = "2016-2018 Serokell";
      maintainer = "Serokell <hi@serokell.io>";
      author = "@serokell";
      homepage = "https://github.com/serokell/log-warper";
      url = "";
      synopsis = "Flexible, configurable, monadic and pretty logging";
      description = "This package implements nice and featureful wrapper around hslogger library.";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."aeson" or (buildDepError "aeson"))
          (hsPkgs."ansi-terminal" or (buildDepError "ansi-terminal"))
          (hsPkgs."containers" or (buildDepError "containers"))
          (hsPkgs."deepseq" or (buildDepError "deepseq"))
          (hsPkgs."directory" or (buildDepError "directory"))
          (hsPkgs."filepath" or (buildDepError "filepath"))
          (hsPkgs."fmt" or (buildDepError "fmt"))
          (hsPkgs."lifted-async" or (buildDepError "lifted-async"))
          (hsPkgs."microlens-platform" or (buildDepError "microlens-platform"))
          (hsPkgs."monad-control" or (buildDepError "monad-control"))
          (hsPkgs."monad-loops" or (buildDepError "monad-loops"))
          (hsPkgs."mmorph" or (buildDepError "mmorph"))
          (hsPkgs."mtl" or (buildDepError "mtl"))
          (hsPkgs."o-clock" or (buildDepError "o-clock"))
          (hsPkgs."text" or (buildDepError "text"))
          (hsPkgs."time" or (buildDepError "time"))
          (hsPkgs."transformers" or (buildDepError "transformers"))
          (hsPkgs."transformers-base" or (buildDepError "transformers-base"))
          (hsPkgs."universum" or (buildDepError "universum"))
          (hsPkgs."unordered-containers" or (buildDepError "unordered-containers"))
          (hsPkgs."vector" or (buildDepError "vector"))
          (hsPkgs."yaml" or (buildDepError "yaml"))
          ] ++ (pkgs.lib).optional (!system.isWindows) (hsPkgs."unix" or (buildDepError "unix"));
        buildable = true;
        };
      exes = {
        "play-log" = {
          depends = [
            (hsPkgs."log-warper" or (buildDepError "log-warper"))
            (hsPkgs."universum" or (buildDepError "universum"))
            (hsPkgs."microlens" or (buildDepError "microlens"))
            (hsPkgs."monad-control" or (buildDepError "monad-control"))
            (hsPkgs."yaml" or (buildDepError "yaml"))
            ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "8.2.2") (hsPkgs."o-clock" or (buildDepError "o-clock"));
          buildable = true;
          };
        "how-to" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."log-warper" or (buildDepError "log-warper"))
            (hsPkgs."markdown-unlit" or (buildDepError "markdown-unlit"))
            (hsPkgs."text" or (buildDepError "text"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.markdown-unlit or (pkgs.buildPackages.markdown-unlit or (buildToolDepError "markdown-unlit")))
            ];
          buildable = true;
          };
        "pure-how-to" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."log-warper" or (buildDepError "log-warper"))
            (hsPkgs."markdown-unlit" or (buildDepError "markdown-unlit"))
            (hsPkgs."mtl" or (buildDepError "mtl"))
            (hsPkgs."text" or (buildDepError "text"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.markdown-unlit or (pkgs.buildPackages.markdown-unlit or (buildToolDepError "markdown-unlit")))
            ];
          buildable = true;
          };
        };
      tests = {
        "log-test" = {
          depends = [
            (hsPkgs."async" or (buildDepError "async"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."data-default" or (buildDepError "data-default"))
            (hsPkgs."directory" or (buildDepError "directory"))
            (hsPkgs."filepath" or (buildDepError "filepath"))
            (hsPkgs."hspec" or (buildDepError "hspec"))
            (hsPkgs."HUnit" or (buildDepError "HUnit"))
            (hsPkgs."microlens-mtl" or (buildDepError "microlens-mtl"))
            (hsPkgs."log-warper" or (buildDepError "log-warper"))
            (hsPkgs."QuickCheck" or (buildDepError "QuickCheck"))
            (hsPkgs."universum" or (buildDepError "universum"))
            (hsPkgs."unordered-containers" or (buildDepError "unordered-containers"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover or (pkgs.buildPackages.hspec-discover or (buildToolDepError "hspec-discover")))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/log-warper";
      rev = "5271ab6c33541b8155ca203e714875974ec116be";
      sha256 = "1h14004a8iwr8nw31pqq6kfdhfsyzzl8a50hrmj2acjqq6mbdl2m";
      });
    }