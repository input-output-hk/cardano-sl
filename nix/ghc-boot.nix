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
      specVersion = "1.22";
      identifier = {
        name = "ghc-boot";
        version = "8.4.4";
      };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "ghc-devs@haskell.org";
      author = "";
      homepage = "";
      url = "";
      synopsis = "Shared functionality between GHC and its boot libraries";
      description = "This library is shared between GHC, ghc-pkg, and other boot\nlibraries.\n\nA note about \"GHC.PackageDb\": it only deals with the subset of\nthe package database that the compiler cares about: modules\npaths etc and not package metadata like description, authors\netc. It is thus not a library interface to ghc-pkg and is *not*\nsuitable for modifying GHC package databases.\n\nThe package database format and this library are constructed in\nsuch a way that while ghc-pkg depends on Cabal, the GHC library\nand program do not have to depend on Cabal.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends  = [
          (hsPkgs.base)
          (hsPkgs.binary)
          (hsPkgs.bytestring)
          (hsPkgs.directory)
          (hsPkgs.filepath)
          (hsPkgs.ghc-boot-th)
        ];
      };
    };
  } // rec { src = pkgs.fetchurl { url = https://s3.eu-central-1.amazonaws.com/ci-static/ghc-cross-windows/ghc-boot-8.4.4.tar.gz; sha256 = "1y72cixzk2zrnha8rqyx9j2q109jb64p1l14g3smvrn9nwwnif2m"; }; }
