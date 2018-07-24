{ system
, compiler
, flags ? {}
, pkgs
, hsPkgs
, pkgconfPkgs }:
  let
    _flags = {} // flags;
  in {
    flags = _flags;
    package = {
      specVersion = "1.8";
      identifier = {
        name = "hfsevents";
        version = "0.1.6";
      };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "stegeman@gmail.com";
      author = "Luite Stegeman";
      homepage = "http://github.com/luite/hfsevents";
      url = "";
      synopsis = "File/folder watching for OS X";
      description = "";
      buildType = "Simple";
    };
    components = {
      "hfsevents" = {
        depends  = [
          (hsPkgs.base)
          (hsPkgs.bytestring)
          (hsPkgs.cereal)
          (hsPkgs.unix)
          (hsPkgs.mtl)
          (hsPkgs.text)
        ];
        libs = [ (pkgs.pthread) ];
        frameworks = [ (pkgs.Cocoa) ];
      };
    };
  } // {
    src = pkgs.fetchgit {
      url = "https://github.com/luite/hfsevents.git";
      rev = "25a53d417d7c7a8fc3116b63e3ba14ca7c8f188f";
      sha256 = "0smpq3yd5m9jd9fpanaqvhadv6qcyp9y5bz0dya0rnxqg909m973";
    };
  }