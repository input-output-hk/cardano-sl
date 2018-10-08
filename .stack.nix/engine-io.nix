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
        name = "engine-io";
        version = "1.2.20";
      };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "ollie@ocharles.org.uk";
      author = "Oliver Charles";
      homepage = "http://github.com/ocharles/engine.io";
      url = "";
      synopsis = "A Haskell implementation of Engine.IO";
      description = "This library provides a Haskell implementation of\n<https://github.com/Automattic/engine.io Engine.IO>, a library for real-time\nclient-server communication on the web. Engine.IO works with old browsers via\nXHR long-polling, and seamlessy upgrades to web sockets. This implementation\nsupports the majority of the Engine.IO protocol, including text and binary\npackets and the upgrading protocol.";
      buildType = "Simple";
    };
    components = {
      "engine-io" = {
        depends  = [
          (hsPkgs.aeson)
          (hsPkgs.async)
          (hsPkgs.attoparsec)
          (hsPkgs.base)
          (hsPkgs.base64-bytestring)
          (hsPkgs.bytestring)
          (hsPkgs.errors)
          (hsPkgs.free)
          (hsPkgs.monad-loops)
          (hsPkgs.mwc-random)
          (hsPkgs.stm)
          (hsPkgs.stm-delay)
          (hsPkgs.text)
          (hsPkgs.transformers)
          (hsPkgs.unordered-containers)
          (hsPkgs.vector)
          (hsPkgs.websockets)
        ];
      };
    };
  } // {
    src = pkgs.fetchgit {
      url = "https://github.com/input-output-hk/engine.io.git";
      rev = "d3c55f51bb81cee7d0d551de930ce65fe7d76756";
      sha256 = "139c0yfnj57cpwg4k0am2rp35sh959394nvlb98011rjy68200qc";
    };
    postUnpack = "sourceRoot+=/engine-io; echo source root reset to \$sourceRoot";
  }