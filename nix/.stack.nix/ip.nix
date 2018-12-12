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
        name = "ip";
        version = "1.3.0";
      };
      license = "BSD-3-Clause";
      copyright = "2016 Andrew Martin";
      maintainer = "andrew.thaddeus@gmail.com";
      author = "Andrew Martin";
      homepage = "https://github.com/andrewthad/haskell-ip#readme";
      url = "";
      synopsis = "Library for IP and MAC addresses";
      description = "The `ip` package provides types and functions for dealing with\nIPv4 addresses, CIDR blocks, and MAC addresses. We provide instances\nfor typeclasses found in commonly used packages like `aeson`, `vector`,\nand `hashable`. We also provide `Parser`s for working with attoparsec.\n\nNotably, this package does not overload functions by introducing any\ntypeclasses of its own. Neither does it prefix functions with the name\nof the type that they work on. Instead, functions of the same name are\nexported by several different modules, and it is expected that end users\ndisambiguate by importing these modules qualified.\n\nThe only module intended to be imported unqualified is `Net.Types`. The\ntypes in this package should not conflict with the types in\nany other commonly used packages.\n\nThe following packages are intended to be used with this package:\n\n* `yesod-ip`: Provides orphan instances needed to work with yesod and\npersistent. Also, provides a `yesod-form` helper.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends  = [
          (hsPkgs.base)
          (hsPkgs.attoparsec)
          (hsPkgs.aeson)
          (hsPkgs.hashable)
          (hsPkgs.text)
          (hsPkgs.bytestring)
          (hsPkgs.vector)
          (hsPkgs.primitive)
        ];
      };
      tests = {
        "test" = {
          depends  = [
            (hsPkgs.base)
            (hsPkgs.ip)
            (hsPkgs.test-framework)
            (hsPkgs.test-framework-quickcheck2)
            (hsPkgs.QuickCheck)
            (hsPkgs.quickcheck-classes)
            (hsPkgs.text)
            (hsPkgs.bytestring)
            (hsPkgs.HUnit)
            (hsPkgs.test-framework-hunit)
            (hsPkgs.attoparsec)
          ];
        };
        "spec" = {
          depends  = [
            (hsPkgs.base)
            (hsPkgs.ip)
            (hsPkgs.hspec)
            (hsPkgs.hspec)
          ];
        };
        "doctest" = {
          depends  = [
            (hsPkgs.base)
            (hsPkgs.ip)
            (hsPkgs.doctest)
            (hsPkgs.QuickCheck)
          ];
        };
      };
      benchmarks = {
        "criterion" = {
          depends  = [
            (hsPkgs.base)
            (hsPkgs.ip)
            (hsPkgs.criterion)
            (hsPkgs.text)
            (hsPkgs.bytestring)
            (hsPkgs.attoparsec)
          ];
        };
      };
    };
  } // {
    src = pkgs.fetchgit {
      url = "https://github.com/andrewthad/haskell-ip";
      rev = "9bb453139aa82cc973125091800422a523e1eb8f";
      sha256 = "199mfpbgca7rvwvwk2zsmcpibc0sk0ni7c5zlf4gk3cps8s85gyr";
    };
  }