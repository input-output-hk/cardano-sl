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
      specVersion = "1.8";
      identifier = {
        name = "stylish-haskell";
        version = "0.9.2.2";
      };
      license = "BSD-3-Clause";
      copyright = "2012 Jasper Van der Jeugt";
      maintainer = "Jasper Van der Jeugt <m@jaspervdj.be>";
      author = "Jasper Van der Jeugt <m@jaspervdj.be>";
      homepage = "https://github.com/jaspervdj/stylish-haskell";
      url = "";
      synopsis = "Haskell code prettifier";
      description = "A Haskell code prettifier. For more information, see:\n\n<https://github.com/jaspervdj/stylish-haskell/blob/master/README.markdown>";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends  = [
          (hsPkgs.aeson)
          (hsPkgs.base)
          (hsPkgs.bytestring)
          (hsPkgs.containers)
          (hsPkgs.directory)
          (hsPkgs.filepath)
          (hsPkgs.file-embed)
          (hsPkgs.haskell-src-exts)
          (hsPkgs.mtl)
          (hsPkgs.semigroups)
          (hsPkgs.syb)
          (hsPkgs.yaml)
        ];
      };
      exes = {
        "stylish-haskell" = {
          depends  = [
            (hsPkgs.stylish-haskell)
            (hsPkgs.strict)
            (hsPkgs.optparse-applicative)
            (hsPkgs.aeson)
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.containers)
            (hsPkgs.directory)
            (hsPkgs.filepath)
            (hsPkgs.file-embed)
            (hsPkgs.haskell-src-exts)
            (hsPkgs.mtl)
            (hsPkgs.syb)
            (hsPkgs.yaml)
          ];
        };
      };
      tests = {
        "stylish-haskell-tests" = {
          depends  = [
            (hsPkgs.HUnit)
            (hsPkgs.test-framework)
            (hsPkgs.test-framework-hunit)
            (hsPkgs.aeson)
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.containers)
            (hsPkgs.directory)
            (hsPkgs.filepath)
            (hsPkgs.file-embed)
            (hsPkgs.haskell-src-exts)
            (hsPkgs.mtl)
            (hsPkgs.syb)
            (hsPkgs.yaml)
          ];
        };
      };
    };
  } // {
    src = pkgs.fetchgit {
      url = "https://github.com/jaspervdj/stylish-haskell";
      rev = "f253f37362be840bafe2c82a8d7fb284046fda48";
      sha256 = "070lwddjvawaf42p8mvxl6fam44nhn98nc85gsk5688fygccv5qw";
    };
  }