{ compiler, flags ? {}, hsPkgs, pkgconfPkgs, pkgs, system }:
let
    _flags = {} // flags;
    in {
      flags = _flags;
      package = {
        specVersion = "1.10";
        identifier = {
          name = "dns";
          version = "3.0.2";
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
          depends  = ([
            hsPkgs.base
            hsPkgs.async
            hsPkgs.auto-update
            hsPkgs.attoparsec
            hsPkgs.base64-bytestring
            hsPkgs.binary
            hsPkgs.bytestring
            hsPkgs.conduit
            hsPkgs.conduit-extra
            hsPkgs.containers
            hsPkgs.cryptonite
            hsPkgs.iproute
            hsPkgs.mtl
            hsPkgs.network
            hsPkgs.psqueues
            hsPkgs.safe
            hsPkgs.time
          ] ++ pkgs.lib.optional (compiler.isGhc && compiler.version.lt "8") hsPkgs.semigroups) ++ pkgs.lib.optional system.isWindows hsPkgs.split;
          libs = pkgs.lib.optional system.isWindows pkgs.iphlpapi;
        };
        tests = {
          network = {
            depends  = [
              hsPkgs.dns
              hsPkgs.base
              hsPkgs.bytestring
              hsPkgs.hspec
            ];
          };
          spec = {
            depends  = [
              hsPkgs.dns
              hsPkgs.QuickCheck
              hsPkgs.base
              hsPkgs.bytestring
              hsPkgs.hspec
              hsPkgs.iproute
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
        rev = "18ebc30692c71f27746a9faf06c6fc78e19bbe7b";
        sha256 = null;
      };
    }