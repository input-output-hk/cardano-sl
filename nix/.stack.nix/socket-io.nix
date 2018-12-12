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
        name = "socket-io";
        version = "1.3.10";
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
      "library" = {
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
      rev = "8f9216b8f9c7bd96cb1feeb82db5271744d67fcd";
      sha256 = "1kamjl01k8njlw6jcwr6nzcd2218wvpk30n5v1f8233hw6qw5x3m";
    };
    postUnpack = "sourceRoot+=/socket-io; echo source root reset to \$sourceRoot";
  }