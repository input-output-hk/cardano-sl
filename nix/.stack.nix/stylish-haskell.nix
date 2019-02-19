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
        version = "0.9.2.0";
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
        depends = [
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
          depends = [
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
          depends = [
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
      url = "https://github.com/avieth/stylish-haskell.git";
      rev = "0bd3e534dcfbad8cc41db773acf623dcc6245d8d";
      sha256 = "11q9gh38bchkhla64l7wghrp6bdp72vh3j8ddf29z7ixx80x2db1";
    };
  }