{ compiler, flags ? {}, hsPkgs, pkgconfPkgs, pkgs, system }:
let
    _flags = {} // flags;
    in {
      flags = _flags;
      package = {
        specVersion = "1.10";
        identifier = {
          name = "OneTuple";
          version = "0.2.2";
        };
        license = "BSD-3-Clause";
        copyright = "(c) John Dorsey 2008";
        maintainer = "John Dorsey <haskell@colquitt.org>";
        author = "John Dorsey <haskell@colquitt.org>";
        homepage = "";
        url = "";
        synopsis = "Singleton Tuple";
        description = "This package provides a singleton tuple data type\n\n> data OneTuple = OneTuple a\n\nNote: it's not a @newtype@";
        buildType = "Simple";
      };
      components = {
        OneTuple = {
          depends  = [
            hsPkgs.base
          ] ++ pkgs.lib.optional (!(compiler.isGhc && compiler.version.ge "8.0")) hsPkgs.semigroups;
        };
      };
    } // {
      src = pkgs.fetchgit {
        url = "https://github.com/phadej/OneTuple.git";
        rev = "e6e789484d41fc00ad8a0c0f947554216ed6d772";
        sha256 = null;
      };
    }