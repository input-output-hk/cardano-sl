{ compiler, flags ? {}, hsPkgs, pkgconfPkgs, pkgs, system }:
let
    _flags = {} // flags;
    in {
      flags = _flags;
      package = {
        specVersion = "1.10";
        identifier = {
          name = "dns";
          version = "3.0.0";
        };
        license = "BSD-3-Clause";
        copyright = "";
        maintainer = "Kazu Yamamoto <kazu@iij.ad.jp>";
        author = "Kazu Yamamoto <kazu@iij.ad.jp>";
        homepage = "";
        url = "";
        synopsis = "DNS library in Haskell";
        description = "A thread-safe DNS library for both clients and servers written\nin pure Haskell.";
        buildType = "Simple";
      };
      components = {
        dns = {
          depends  = (pkgs.lib.optional (compiler.isGhc && compiler.version.lt "8") hsPkgs.semigroups ++ (if compiler.isGhc && compiler.version.ge "7"
            then [
              hsPkgs.base
              hsPkgs.attoparsec
              hsPkgs.base64-bytestring
              hsPkgs.binary
              hsPkgs.bytestring
              hsPkgs.bytestring-builder
              hsPkgs.conduit
              hsPkgs.conduit-extra
              hsPkgs.containers
              hsPkgs.iproute
              hsPkgs.mtl
              hsPkgs.network
              hsPkgs.random
              hsPkgs.resourcet
              hsPkgs.safe
            ]
            else [
              hsPkgs.base
              hsPkgs.attoparsec
              hsPkgs.base64-bytestring
              hsPkgs.binary
              hsPkgs.bytestring
              hsPkgs.bytestring-builder
              hsPkgs.conduit
              hsPkgs.conduit-extra
              hsPkgs.containers
              hsPkgs.iproute
              hsPkgs.mtl
              hsPkgs.network
              hsPkgs.network-bytestring
              hsPkgs.random
              hsPkgs.resourcet
              hsPkgs.safe
            ])) ++ pkgs.lib.optional system.isWindows hsPkgs.split;
          libs = pkgs.lib.optional system.isWindows pkgs.iphlpapi;
        };
        tests = {
          network = {
            depends  = [
              hsPkgs.dns
              hsPkgs.base
              hsPkgs.bytestring
              hsPkgs.bytestring-builder
              hsPkgs.hspec
            ];
          };
          spec = {
            depends  = [
              hsPkgs.base
              hsPkgs.attoparsec
              hsPkgs.binary
              hsPkgs.bytestring
              hsPkgs.bytestring-builder
              hsPkgs.conduit
              hsPkgs.conduit-extra
              hsPkgs.containers
              hsPkgs.dns
              hsPkgs.hspec
              hsPkgs.iproute
              hsPkgs.mtl
              hsPkgs.network
              hsPkgs.QuickCheck
              hsPkgs.random
              hsPkgs.resourcet
              hsPkgs.safe
              hsPkgs.word8
            ];
          };
          doctest = {
            depends  = [
              hsPkgs.base
              hsPkgs.doctest
            ];
          };
        };
      };
    } // {
      src = pkgs.fetchgit {
        url = "https://github.com/kazu-yamamoto/dns.git";
        rev = "b106470f0a93672af22cbc7ed6564b53c0f249ed";
        sha256 = null;
      };
    }