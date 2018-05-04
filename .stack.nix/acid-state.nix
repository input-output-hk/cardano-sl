{ compiler, flags ? {}, hsPkgs, pkgconfPkgs, pkgs, system }:
let
    _flags = {} // flags;
    in {
      flags = _flags;
      package = {
        specVersion = "1.10";
        identifier = {
          name = "acid-state";
          version = "0.14.2";
        };
        license = "LicenseRef-PublicDomain";
        copyright = "";
        maintainer = "Lemmih <lemmih@gmail.com>";
        author = "David Himmelstrup";
        homepage = "https://github.com/acid-state/acid-state";
        url = "";
        synopsis = "Add ACID guarantees to any serializable Haskell data structure.";
        description = "Use regular Haskell data structures as your database and get stronger ACID guarantees than most RDBMS offer.";
        buildType = "Simple";
      };
      components = {
        acid-state = {
          depends  = [
            hsPkgs.array
            hsPkgs.base
            hsPkgs.bytestring
            hsPkgs.cereal
            hsPkgs.containers
            hsPkgs.directory
            hsPkgs.filelock
            hsPkgs.filepath
            hsPkgs.exceptions
            hsPkgs.extensible-exceptions
            hsPkgs.extra
            hsPkgs.hashable
            hsPkgs.mtl
            hsPkgs.network
            hsPkgs.safecopy
            hsPkgs.stm
            hsPkgs.template-haskell
            hsPkgs.th-expand-syns
            hsPkgs.time-units
            hsPkgs.unordered-containers
          ] ++ (if system.isWindows
            then [ hsPkgs.Win32 ]
            else [ hsPkgs.unix ]);
        };
        benchmarks = {
          loading-benchmark = {
            depends  = [
              hsPkgs.random
              hsPkgs.directory
              hsPkgs.system-fileio
              hsPkgs.system-filepath
              hsPkgs.criterion
              hsPkgs.mtl
              hsPkgs.base
              hsPkgs.acid-state
            ];
          };
        };
      };
    } // {
      src = pkgs.fetchgit {
        url = "https://github.com/serokell/acid-state.git";
        rev = "1049699df411c9584523ba7424cba1f3f82ac419";
        sha256 = null;
      };
    }