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
  ({
    flags = {};
    package = {
      specVersion = "0";
      identifier = { name = "inspector"; version = "0.2"; };
      license = "BSD-3-Clause";
      copyright = "2017-2018 PrimeType Ltd";
      maintainer = "nicolas@primetype.co.uk";
      author = "Nicolas Di Prima";
      homepage = "https://github.com/primetype/inspector#readme";
      url = "";
      synopsis = "";
      description = "";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."basement" or (buildDepError "basement"))
          (hsPkgs."cryptonite" or (buildDepError "cryptonite"))
          (hsPkgs."foundation" or (buildDepError "foundation"))
          (hsPkgs."memory" or (buildDepError "memory"))
          ];
        buildable = true;
        };
      exes = {
        "example" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."basement" or (buildDepError "basement"))
            (hsPkgs."cryptonite" or (buildDepError "cryptonite"))
            (hsPkgs."foundation" or (buildDepError "foundation"))
            (hsPkgs."inspector" or (buildDepError "inspector"))
            (hsPkgs."memory" or (buildDepError "memory"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/primetype/inspector.git";
      rev = "c975f4329365f0379c04358138e616fb96fb0b79";
      sha256 = "12q1v7a8kcw7qi4lws4j3mvxwfkhni6zmp870kmnkgbgwvrax9gs";
      });
    }) // { cabal-generator = "hpack"; }