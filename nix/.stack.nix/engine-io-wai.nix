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
      specVersion = "1.10";
      identifier = {
        name = "engine-io-wai";
        version = "1.0.8";
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
      "engine-io-wai" = {
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
      rev = "d3c55f51bb81cee7d0d551de930ce65fe7d76756";
      sha256 = "139c0yfnj57cpwg4k0am2rp35sh959394nvlb98011rjy68200qc";
    };
    postUnpack = "sourceRoot+=/engine-io-wai; echo source root reset to \$sourceRoot";
  }