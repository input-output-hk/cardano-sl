{ compiler, flags ? {}, hsPkgs, pkgconfPkgs, pkgs, system }:
let
    _flags = {} // flags;
    in {
      flags = _flags;
      package = {
        specVersion = "1.6";
        identifier = {
          name = "time-units";
          version = "1.0.0";
        };
        license = "BSD-3-Clause";
        copyright = "";
        maintainer = "Adam Wick <awick@uhusre.com>";
        author = "Adam Wick <awick@uhsure.com>";
        homepage = "http://github.com/acw/time-units";
        url = "";
        synopsis = "A basic library for defining units of time as types.";
        description = "In many cases, it is useful (either for error checking or documentation\nreasons) to define input and output types as having a particular unit of\ntime. In addition, by creating a type class defining type units, this\nlibrary should make it easier to separate the units of time the developer\nwants to think in versus the units of time the library author wants to\nthink in.";
        buildType = "Simple";
      };
      components = {
        time-units = {
          depends  = [ hsPkgs.base ];
        };
      };
    } // {
      src = pkgs.fetchgit {
        url = "https://github.com/serokell/time-units.git";
        rev = "6c3747c1ac794f952de996dd7ba8a2f6d63bf132";
        sha256 = null;
      };
    }