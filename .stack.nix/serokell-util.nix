{ compiler, flags ? {}, hsPkgs, pkgconfPkgs, pkgs, system }:
let
    _flags = {} // flags;
    in {
      flags = _flags;
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
        serokell-util = {
          depends  = [
            hsPkgs.base
            hsPkgs.aeson
            hsPkgs.ansi-terminal
            hsPkgs.base16-bytestring
            hsPkgs.base64-bytestring
            hsPkgs.bytestring
            hsPkgs.clock
            hsPkgs.deepseq
            hsPkgs.exceptions
            hsPkgs.fmt
            hsPkgs.formatting
            hsPkgs.hashable
            hsPkgs.microlens
            hsPkgs.microlens-mtl
            hsPkgs.mtl
            hsPkgs.o-clock
            hsPkgs.parsec
            hsPkgs.process
            hsPkgs.QuickCheck
            hsPkgs.quickcheck-instances
            hsPkgs.scientific
            hsPkgs.template-haskell
            hsPkgs.text
            hsPkgs.th-lift-instances
            hsPkgs.transformers
            hsPkgs.universum
            hsPkgs.unordered-containers
            hsPkgs.vector
          ];
        };
        tests = {
          serokell-test = {
            depends  = [
              hsPkgs.aeson
              hsPkgs.base
              hsPkgs.extra
              hsPkgs.hspec
              hsPkgs.QuickCheck
              hsPkgs.quickcheck-instances
              hsPkgs.scientific
              hsPkgs.serokell-util
              hsPkgs.universum
              hsPkgs.unordered-containers
              hsPkgs.vector
            ];
          };
        };
      };
    } // {
      src = pkgs.fetchgit {
        url = "https://github.com/angerman/serokell-util.git";
        rev = "573a6f7f04d1d3a448b1eb509836ff890c270258";
        sha256 = null;
      };
    }