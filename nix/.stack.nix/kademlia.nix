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
      identifier = { name = "kademlia"; version = "1.1.0.1"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "fro_ozen <fro_ozen@gmx.de>, 2016 Serokell <hi@serokell.io>";
      author = "fro_ozen <fro_ozen@gmx.de>";
      homepage = "https://github.com/serokell/kademlia";
      url = "";
      synopsis = "An implementation of the Kademlia DHT Protocol";
      description = "\nA haskell implementation of the Kademlia distributed hashtable, an efficient\nway to store and lookup values distributed over a P2P network.\n\nThe implementation is based on the paper\n/Kademlia: A Peer-to-peer Information System Based on the XOR Metric/:\n<http://pdos.csail.mit.edu/~petar/papers/maymounkov-kademlia-lncs.pdf>\nby Petar Maymounkov and David Mazières.\n\nThis library aims to be very simple and pleasant to use, with the downside of\ndeciding some of the implementation details, like timeout intervals and\nk-bucket size, for the user.";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."containers" or (buildDepError "containers"))
          (hsPkgs."extra" or (buildDepError "extra"))
          (hsPkgs."memory" or (buildDepError "memory"))
          (hsPkgs."MonadRandom" or (buildDepError "MonadRandom"))
          (hsPkgs."mtl" or (buildDepError "mtl"))
          (hsPkgs."network" or (buildDepError "network"))
          (hsPkgs."random" or (buildDepError "random"))
          (hsPkgs."random-shuffle" or (buildDepError "random-shuffle"))
          (hsPkgs."stm" or (buildDepError "stm"))
          (hsPkgs."time" or (buildDepError "time"))
          (hsPkgs."transformers" or (buildDepError "transformers"))
          (hsPkgs."cryptonite" or (buildDepError "cryptonite"))
          (hsPkgs."contravariant" or (buildDepError "contravariant"))
          ];
        buildable = true;
        };
      exes = {
        "discovery-test" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."binary" or (buildDepError "binary"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."data-default" or (buildDepError "data-default"))
            (hsPkgs."extra" or (buildDepError "extra"))
            (hsPkgs."kademlia" or (buildDepError "kademlia"))
            (hsPkgs."MonadRandom" or (buildDepError "MonadRandom"))
            (hsPkgs."mtl" or (buildDepError "mtl"))
            (hsPkgs."network" or (buildDepError "network"))
            (hsPkgs."random" or (buildDepError "random"))
            (hsPkgs."random-shuffle" or (buildDepError "random-shuffle"))
            (hsPkgs."transformers" or (buildDepError "transformers"))
            (hsPkgs."transformers-compat" or (buildDepError "transformers-compat"))
            ];
          buildable = true;
          };
        };
      tests = {
        "library-test" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."binary" or (buildDepError "binary"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."data-default" or (buildDepError "data-default"))
            (hsPkgs."errors" or (buildDepError "errors"))
            (hsPkgs."extra" or (buildDepError "extra"))
            (hsPkgs."HUnit" or (buildDepError "HUnit"))
            (hsPkgs."kademlia" or (buildDepError "kademlia"))
            (hsPkgs."MonadRandom" or (buildDepError "MonadRandom"))
            (hsPkgs."mtl" or (buildDepError "mtl"))
            (hsPkgs."network" or (buildDepError "network"))
            (hsPkgs."QuickCheck" or (buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-instances" or (buildDepError "quickcheck-instances"))
            (hsPkgs."random" or (buildDepError "random"))
            (hsPkgs."random-shuffle" or (buildDepError "random-shuffle"))
            (hsPkgs."stm" or (buildDepError "stm"))
            (hsPkgs."tasty" or (buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (buildDepError "tasty-quickcheck"))
            (hsPkgs."time" or (buildDepError "time"))
            (hsPkgs."transformers" or (buildDepError "transformers"))
            (hsPkgs."transformers-compat" or (buildDepError "transformers-compat"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/serokell/kademlia.git";
      rev = "7120bb4d28e708acd52dfd61d3dca7914fac7d7f";
      sha256 = "1k1wp9dwhzzqfivxc28vhxfqplnyh916crr7bhsiv829d6qifhw1";
      });
    }