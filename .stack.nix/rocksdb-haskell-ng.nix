{ compiler, flags ? {}, hsPkgs, pkgconfPkgs, pkgs, system }:
let
    _flags = {} // flags;
    in {
      flags = _flags;
      package = {
        specVersion = "1.10";
        identifier = {
          name = "rocksdb-haskell-ng";
          version = "0.0.0";
        };
        license = "BSD-3-Clause";
        copyright = "";
        maintainer = "";
        author = "";
        homepage = "";
        url = "";
        synopsis = "";
        description = "";
        buildType = "Simple";
      };
      components = {
        rocksdb-haskell-ng = {
          depends  = [
            hsPkgs.base
            hsPkgs.bytestring
            hsPkgs.directory
          ];
          libs = [ pkgs.rocksdb ];
        };
        tests = {
          test = {
            depends  = [
              hsPkgs.base
              hsPkgs.rocksdb-haskell-ng
              hsPkgs.hspec
              hsPkgs.QuickCheck
              hsPkgs.temporary
              hsPkgs.directory
              hsPkgs.filepath
              hsPkgs.deepseq
            ];
          };
        };
        benchmarks = {
          speed = {
            depends  = [
              hsPkgs.base
              hsPkgs.rocksdb-haskell-ng
              hsPkgs.criterion
              hsPkgs.hspec
              hsPkgs.QuickCheck
              hsPkgs.temporary
              hsPkgs.directory
              hsPkgs.filepath
              hsPkgs.deepseq
            ];
          };
        };
      };
    } // {
      src = pkgs.fetchgit {
        url = "https://github.com/input-output-hk/rocksdb-haskell-ng.git";
        rev = "49f501a082d745f3b880677220a29cafaa181452";
        sha256 = null;
      };
    }