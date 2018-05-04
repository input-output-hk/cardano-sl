{ compiler, flags ? {}, hsPkgs, pkgconfPkgs, pkgs, system }:
let
    _flags = {} // flags;
    in {
      flags = _flags;
      package = {
        specVersion = "1.10";
        identifier = {
          name = "diagrams-postscript";
          version = "1.5";
        };
        license = "BSD-3-Clause";
        copyright = "";
        maintainer = "diagrams-discuss@googlegroups.com";
        author = "Ryan Yates";
        homepage = "http://projects.haskell.org/diagrams/";
        url = "";
        synopsis = "Postscript backend for diagrams drawing EDSL";
        description = "This package provides a modular backend for rendering\ndiagrams created with the diagrams EDSL using Postscript.\n\n* \"Diagrams.Backend.Postscript.CmdLine\" - Provides\nthe \"mainWith\" interface to render a diagram\nbased on command line options.\n\n* \"Diagrams.Backend.Postscript\" - Provides the\ngeneral API for rendering diagrams using the\nPostscript backend.\n\n* \"Diagrams.Backend.CMYK\" - Special support for CMYK\ncolor attributes.";
        buildType = "Simple";
      };
      components = {
        diagrams-postscript = {
          depends  = [
            hsPkgs.base
            hsPkgs.bytestring
            hsPkgs.mtl
            hsPkgs.filepath
            hsPkgs.diagrams-core
            hsPkgs.diagrams-lib
            hsPkgs.data-default-class
            hsPkgs.statestack
            hsPkgs.split
            hsPkgs.monoid-extras
            hsPkgs.semigroups
            hsPkgs.lens
            hsPkgs.containers
            hsPkgs.hashable
          ] ++ pkgs.lib.optional (compiler.isGhc && compiler.version.lt "7.6") hsPkgs.ghc-prim;
        };
      };
    } // {
      src = pkgs.fetchgit {
        url = "https://github.com/diagrams/diagrams-postscript.git";
        rev = "913e1bb83a3ed1213b1da6c3777e0096df562edb";
        sha256 = null;
      };
    }