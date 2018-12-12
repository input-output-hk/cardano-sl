{ system
, compiler
, flags
, pkgs
, hsPkgs
, pkgconfPkgs
, ... }:
  {
    flags = {
      use-mock-network = false;
    };
    package = {
      specVersion = "1.10";
      identifier = {
        name = "network-transport-tcp";
        version = "0.6.0";
      };
      license = "BSD-3-Clause";
      copyright = "Well-Typed LLP, Tweag I/O Limited";
      maintainer = "Facundo Domínguez <facundo.dominguez@tweag.io>";
      author = "Duncan Coutts, Nicolas Wu, Edsko de Vries";
      homepage = "http://haskell-distributed.github.com";
      url = "";
      synopsis = "TCP instantiation of Network.Transport";
      description = "TCP instantiation of Network.Transport";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends  = [
          (hsPkgs.base)
          (hsPkgs.network-transport)
          (hsPkgs.data-accessor)
          (hsPkgs.containers)
          (hsPkgs.bytestring)
          (hsPkgs.network)
          (hsPkgs.uuid)
        ];
      };
      tests = {
        "TestTCP" = {
          depends  = [
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.network-transport-tests)
            (hsPkgs.network)
            (hsPkgs.network-transport)
            (hsPkgs.network-transport-tcp)
          ];
        };
        "TestQC" = {
          depends  = pkgs.lib.optionals (flags.use-mock-network) [
            (hsPkgs.base)
            (hsPkgs.test-framework)
            (hsPkgs.test-framework-quickcheck2)
            (hsPkgs.test-framework-hunit)
            (hsPkgs.QuickCheck)
            (hsPkgs.HUnit)
            (hsPkgs.network-transport)
            (hsPkgs.network-transport-tcp)
            (hsPkgs.containers)
            (hsPkgs.bytestring)
            (hsPkgs.pretty)
            (hsPkgs.data-accessor)
            (hsPkgs.data-accessor-transformers)
            (hsPkgs.mtl)
            (hsPkgs.transformers)
            (hsPkgs.lockfree-queue)
          ];
        };
      };
    };
  } // {
    src = pkgs.fetchgit {
      url = "https://github.com/serokell/network-transport-tcp";
      rev = "3d56652123bd296dc759cd31947eb2a17924e68a";
      sha256 = "1cim8867kf36kv6ax9jx5shlqd7hhs1i4jf101x4pk3wriwbxsds";
    };
  }