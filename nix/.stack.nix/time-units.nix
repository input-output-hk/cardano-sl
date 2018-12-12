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
      "library" = {
        depends  = [ (hsPkgs.base) ];
      };
    };
  } // {
    src = pkgs.fetchgit {
      url = "https://github.com/serokell/time-units.git";
      rev = "6c3747c1ac794f952de996dd7ba8a2f6d63bf132";
      sha256 = "0psdr1if0rgnn24698x3583m0603rwd3sd7yb9whj03hskmkwpgs";
    };
  }