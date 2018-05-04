{ compiler, flags ? {}, hsPkgs, pkgconfPkgs, pkgs, system }:
let
    _flags = {} // flags;
    in {
      flags = _flags;
      package = {
        specVersion = "1.18";
        identifier = {
          name = "universum";
          version = "1.1.1";
        };
        license = "MIT";
        copyright = "2016 Stephen Diehl, 2016-2018 Serokell";
        maintainer = "Serokell <hi@serokell.io>";
        author = "Stephen Diehl, @serokell";
        homepage = "https://github.com/serokell/universum";
        url = "";
        synopsis = "Custom prelude used in Serokell";
        description = "See README.md file for more details.";
        buildType = "Simple";
      };
      components = {
        universum = {
          depends  = [
            hsPkgs.base
            hsPkgs.bytestring
            hsPkgs.containers
            hsPkgs.deepseq
            hsPkgs.ghc-prim
            hsPkgs.hashable
            hsPkgs.microlens
            hsPkgs.microlens-mtl
            hsPkgs.mtl
            hsPkgs.safe-exceptions
            hsPkgs.stm
            hsPkgs.text
            hsPkgs.formatting
            hsPkgs.transformers
            hsPkgs.type-operators
            hsPkgs.unordered-containers
            hsPkgs.utf8-string
            hsPkgs.vector
          ];
        };
        tests = {
          universum-test = {
            depends  = [
              hsPkgs.base
              hsPkgs.universum
              hsPkgs.bytestring
              hsPkgs.text
              hsPkgs.utf8-string
              hsPkgs.hedgehog
              hsPkgs.tasty
              hsPkgs.tasty-hedgehog
            ];
          };
          universum-doctest = {
            depends  = [
              hsPkgs.base
              hsPkgs.doctest
              hsPkgs.Glob
            ];
          };
        };
        benchmarks = {
          universum-benchmark = {
            depends  = [
              hsPkgs.base
              hsPkgs.universum
              hsPkgs.containers
              hsPkgs.gauge
              hsPkgs.unordered-containers
            ] ++ pkgs.lib.optional (compiler.isGhc && compiler.version.eq "7.10.3") hsPkgs.semigroups;
          };
        };
      };
    } // {
      src = pkgs.fetchgit {
        url = "https://github.com/angerman/universum.git";
        rev = "edda5f6e52c642d8625ef22e16e6d737b81d9c54";
        sha256 = null;
      };
    }