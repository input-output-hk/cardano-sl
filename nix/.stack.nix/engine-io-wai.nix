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
      specVersion = "1.10";
      identifier = {
        name = "engine-io-wai";
        version = "1.0.9";
      };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "brandon@codedmart.com";
      author = "Brandon Martin";
      homepage = "http://github.com/ocharles/engine.io";
      url = "";
      synopsis = "An @engine-io@ @ServerAPI@ that is compatible with @Wai@";
      description = "This package provides an @engine-io@ @ServerAPI@ that is compatible with\n<https://hackage.haskell.org/package/wai/ Wai>.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends  = [
          (hsPkgs.base)
          (hsPkgs.engine-io)
          (hsPkgs.http-types)
          (hsPkgs.unordered-containers)
          (hsPkgs.wai)
          (hsPkgs.text)
          (hsPkgs.bytestring)
          (hsPkgs.websockets)
          (hsPkgs.wai-websockets)
          (hsPkgs.mtl)
          (hsPkgs.either)
          (hsPkgs.transformers)
          (hsPkgs.transformers-compat)
          (hsPkgs.attoparsec)
        ];
      };
    };
  } // {
    src = pkgs.fetchgit {
      url = "https://github.com/input-output-hk/engine.io.git";
      rev = "8f9216b8f9c7bd96cb1feeb82db5271744d67fcd";
      sha256 = "1kamjl01k8njlw6jcwr6nzcd2218wvpk30n5v1f8233hw6qw5x3m";
    };
    postUnpack = "sourceRoot+=/engine-io-wai; echo source root reset to \$sourceRoot";
  }