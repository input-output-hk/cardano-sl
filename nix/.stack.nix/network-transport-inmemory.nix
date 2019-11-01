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
      specVersion = "1.8";
      identifier = { name = "network-transport-inmemory"; version = "0.5.1"; };
      license = "BSD-3-Clause";
      copyright = "Well-Typed LLP, Tweag I/O Limited";
      maintainer = "Facundo Domínguez <facundo.dominguez@tweag.io>";
      author = "Duncan Coutts, Nicolas Wu, Edsko de Vries, Alexander Vershilov";
      homepage = "http://haskell-distributed.github.com";
      url = "";
      synopsis = "In-memory instantiation of Network.Transport";
      description = "This is a transport implementation that could be used for local\ncommunication in the same address space (i.e. one process).\n\nIt could be used either for testing purposes or for local\ncommunication that require the network-transport semantics.\n\nNB: network-tranpsport-inmemory does not support cross-transport\ncommunication. All endpoints that want to comminicate should be\ncreated using the same transport.";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."network-transport" or (buildDepError "network-transport"))
          (hsPkgs."data-accessor" or (buildDepError "data-accessor"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."containers" or (buildDepError "containers"))
          (hsPkgs."stm" or (buildDepError "stm"))
          ];
        buildable = true;
        };
      tests = {
        "TestMulticastInMemory" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."network-transport-inmemory" or (buildDepError "network-transport-inmemory"))
            (hsPkgs."network-transport" or (buildDepError "network-transport"))
            (hsPkgs."network-transport-tests" or (buildDepError "network-transport-tests"))
            ];
          buildable = false;
          };
        "TestInMemory" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."network-transport-inmemory" or (buildDepError "network-transport-inmemory"))
            (hsPkgs."network-transport-tests" or (buildDepError "network-transport-tests"))
            (hsPkgs."network-transport" or (buildDepError "network-transport"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/avieth/network-transport-inmemory";
      rev = "5d8ff2b07b9df35cf61329a3d975e2c8cf95c12a";
      sha256 = "0ak64rks3lk3kk5wyndrrk2swmd84h9diribzix305xwz1jhjj9w";
      });
    }