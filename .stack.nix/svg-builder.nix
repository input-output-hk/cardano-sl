{ compiler, flags ? {}, hsPkgs, pkgconfPkgs, pkgs, system }:
let
    _flags = {} // flags;
    in {
      flags = _flags;
      package = {
        specVersion = "1.10";
        identifier = {
          name = "svg-builder";
          version = "0.1.0.2";
        };
        license = "BSD-3-Clause";
        copyright = "2016 Jeffrey Rosenbluth";
        maintainer = "jeffrey.rosenbluth@gmail.com";
        author = "Jeffrey Rosenbluth";
        homepage = "https://github.com/diagrams/svg-builder.git";
        url = "";
        synopsis = "DSL for building SVG.";
        description = "Fast, easy to write SVG.";
        buildType = "Simple";
      };
      components = {
        svg-builder = {
          depends  = [
            hsPkgs.base
            hsPkgs.blaze-builder
            hsPkgs.bytestring
            hsPkgs.hashable
            hsPkgs.text
            hsPkgs.unordered-containers
          ] ++ pkgs.lib.optional (!(compiler.isGhc && compiler.version.ge "8.0")) hsPkgs.semigroups;
        };
      };
    } // {
      src = pkgs.fetchgit {
        url = "https://github.com/diagrams/svg-builder.git";
        rev = "29cbcb3b03727ef1af08a8c262bb7e2c2d6cbc46";
        sha256 = null;
      };
    }