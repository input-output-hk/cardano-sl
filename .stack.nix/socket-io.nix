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
        name = "socket-io";
        version = "1.3.9";
      };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "ollie@ocharles.org.uk";
      author = "Oliver Charles";
      homepage = "http://github.com/ocharles/engine.io";
      url = "";
      synopsis = "";
      description = "This library provides an implementation of <http://socket.io Socket.io>\nprotocol (version 1). It builds on top of Engine.IO, allowing Socket.io to\nwork with both long polling XHR requests, and seamlessly upgrading them to\nHTML 5 web sockets.";
      buildType = "Simple";
    };
    components = {
      "socket-io" = {
        depends  = [
          (hsPkgs.aeson)
          (hsPkgs.attoparsec)
          (hsPkgs.base)
          (hsPkgs.bytestring)
          (hsPkgs.engine-io)
          (hsPkgs.mtl)
          (hsPkgs.stm)
          (hsPkgs.text)
          (hsPkgs.transformers)
          (hsPkgs.unordered-containers)
          (hsPkgs.vector)
        ];
      };
    };
  } // {
    src = pkgs.fetchgit {
      url = "https://github.com/input-output-hk/engine.io.git";
      rev = "d3c55f51bb81cee7d0d551de930ce65fe7d76756";
      sha256 = "139c0yfnj57cpwg4k0am2rp35sh959394nvlb98011rjy68200qc";
    };
    postUnpack = "sourceRoot+=/socket-io; echo source root reset to \$sourceRoot";
  }