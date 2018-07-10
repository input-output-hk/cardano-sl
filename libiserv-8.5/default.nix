{ compiler, flags ? {}, hsPkgs, pkgconfPkgs, pkgs, system }:
let
    _flags = {
      network = false;
    } // flags;
    in {
      flags = _flags;
      package = {
        specVersion = "1.10";
        identifier = {
          name = "libiserv";
          version = "8.5";
        };
        license = "BSD-3-Clause";
        copyright = "XXX";
        maintainer = "XXX";
        author = "XXX";
        homepage = "";
        url = "";
        synopsis = "Provides shared functionality between iserv and iserv-proxy";
        description = "";
        buildType = "Simple";
      };
      components = {
        libiserv = {
          depends  = ([
            hsPkgs.base
            hsPkgs.binary
            hsPkgs.bytestring
            hsPkgs.containers
            hsPkgs.deepseq
            hsPkgs.ghci
          ] ++ pkgs.lib.optionals _flags.network [
            hsPkgs.network
            hsPkgs.directory
            hsPkgs.filepath
          ]) ++ pkgs.lib.optional (!system.isWindows) hsPkgs.unix;
        };
      };
    } // rec { src = ./.; }
