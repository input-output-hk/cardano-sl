{ system
, compiler
, flags
, pkgs
, hsPkgs
, pkgconfPkgs
, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = {
        name = "kademlia";
        version = "1.1.0.1";
      };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "fro_ozen <fro_ozen@gmx.de>, 2016 Serokell <hi@serokell.io>";
      author = "fro_ozen <fro_ozen@gmx.de>";
      homepage = "https://github.com/serokell/kademlia";
      url = "";
      synopsis = "An implementation of the Kademlia DHT Protocol";
      description = "\nA haskell implementation of the Kademlia distributed hashtable, an efficient\nway to store and lookup values distributed over a P2P network.\n\nThe implementation is based on the paper\n/Kademlia: A Peer-to-peer Information System Based on the XOR Metric/:\n<http://pdos.csail.mit.edu/~petar/papers/maymounkov-kademlia-lncs.pdf>\nby Petar Maymounkov and David Mazières.\n\nThis library aims to be very simple and pleasant to use, with the downside of\ndeciding some of the implementation details, like timeout intervals and\nk-bucket size, for the user.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends  = [
          (hsPkgs.base)
          (hsPkgs.bytestring)
          (hsPkgs.containers)
          (hsPkgs.extra)
          (hsPkgs.memory)
          (hsPkgs.MonadRandom)
          (hsPkgs.mtl)
          (hsPkgs.network)
          (hsPkgs.random)
          (hsPkgs.random-shuffle)
          (hsPkgs.stm)
          (hsPkgs.time)
          (hsPkgs.transformers)
          (hsPkgs.cryptonite)
          (hsPkgs.contravariant)
        ];
      };
      exes = {
        "discovery-test" = {
          depends  = [
            (hsPkgs.base)
            (hsPkgs.binary)
            (hsPkgs.bytestring)
            (hsPkgs.containers)
            (hsPkgs.data-default)
            (hsPkgs.extra)
            (hsPkgs.kademlia)
            (hsPkgs.MonadRandom)
            (hsPkgs.mtl)
            (hsPkgs.network)
            (hsPkgs.random)
            (hsPkgs.random-shuffle)
            (hsPkgs.transformers)
            (hsPkgs.transformers-compat)
          ];
        };
      };
      tests = {
        "library-test" = {
          depends  = [
            (hsPkgs.base)
            (hsPkgs.binary)
            (hsPkgs.bytestring)
            (hsPkgs.containers)
            (hsPkgs.data-default)
            (hsPkgs.errors)
            (hsPkgs.extra)
            (hsPkgs.HUnit)
            (hsPkgs.kademlia)
            (hsPkgs.MonadRandom)
            (hsPkgs.mtl)
            (hsPkgs.network)
            (hsPkgs.QuickCheck)
            (hsPkgs.quickcheck-instances)
            (hsPkgs.random)
            (hsPkgs.random-shuffle)
            (hsPkgs.stm)
            (hsPkgs.tasty)
            (hsPkgs.tasty-hunit)
            (hsPkgs.tasty-quickcheck)
            (hsPkgs.time)
            (hsPkgs.transformers)
            (hsPkgs.transformers-compat)
          ];
        };
      };
    };
  } // {
    src = pkgs.fetchgit {
      url = "https://github.com/serokell/kademlia.git";
      rev = "7120bb4d28e708acd52dfd61d3dca7914fac7d7f";
      sha256 = "1k1wp9dwhzzqfivxc28vhxfqplnyh916crr7bhsiv829d6qifhw1";
    };
  }