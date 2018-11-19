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
      specVersion = "2.0";
      identifier = {
        name = "serokell-util";
        version = "0.9.0";
      };
      license = "MIT";
      copyright = "2016-2018 Serokell";
      maintainer = "Serokell <hi@serokell.io>";
      author = "Serokell";
      homepage = "https://github.com/serokell/serokell-util";
      url = "";
      synopsis = "General-purpose functions by Serokell";
      description = "Serokell-util is a library consisting of functions, which\nare not included in standard libraries, but are useful for\nmultiple projects. This library was created when it was\nfound that in new projects we need to use some utility\nfunctions from existing projects and don't want to\ncopy-paste them.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends  = [
          (hsPkgs.base)
          (hsPkgs.aeson)
          (hsPkgs.ansi-terminal)
          (hsPkgs.base16-bytestring)
          (hsPkgs.base64-bytestring)
          (hsPkgs.bytestring)
          (hsPkgs.clock)
          (hsPkgs.deepseq)
          (hsPkgs.exceptions)
          (hsPkgs.fmt)
          (hsPkgs.formatting)
          (hsPkgs.hashable)
          (hsPkgs.microlens)
          (hsPkgs.microlens-mtl)
          (hsPkgs.mtl)
          (hsPkgs.o-clock)
          (hsPkgs.parsec)
          (hsPkgs.process)
          (hsPkgs.QuickCheck)
          (hsPkgs.quickcheck-instances)
          (hsPkgs.scientific)
          (hsPkgs.template-haskell)
          (hsPkgs.text)
          (hsPkgs.th-lift-instances)
          (hsPkgs.transformers)
          (hsPkgs.universum)
          (hsPkgs.unordered-containers)
          (hsPkgs.vector)
        ];
      };
      tests = {
        "serokell-test" = {
          depends  = [
            (hsPkgs.aeson)
            (hsPkgs.base)
            (hsPkgs.extra)
            (hsPkgs.hspec)
            (hsPkgs.QuickCheck)
            (hsPkgs.quickcheck-instances)
            (hsPkgs.scientific)
            (hsPkgs.serokell-util)
            (hsPkgs.universum)
            (hsPkgs.unordered-containers)
            (hsPkgs.vector)
          ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover)
          ];
        };
      };
    };
  } // {
    src = pkgs.fetchgit {
      url = "https://github.com/input-output-hk/serokell-util.git";
      rev = "4ead9809e119483e7832da5f8224b0c4d4a2d5d6";
      sha256 = "0h4wcvp126w021bc2kglsbfywyvc9z3yf1sh1k4yy1an4ckh51jd";
    };
  }