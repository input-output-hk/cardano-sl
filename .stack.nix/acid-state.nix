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
        name = "acid-state";
        version = "0.15.0";
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
      "acid-state" = {
        depends  = [
          (hsPkgs.array)
          (hsPkgs.base)
          (hsPkgs.bytestring)
          (hsPkgs.cereal)
          (hsPkgs.containers)
          (hsPkgs.extensible-exceptions)
          (hsPkgs.safecopy)
          (hsPkgs.stm)
          (hsPkgs.directory)
          (hsPkgs.filelock)
          (hsPkgs.filepath)
          (hsPkgs.mtl)
          (hsPkgs.network)
          (hsPkgs.template-haskell)
          (hsPkgs.th-expand-syns)
        ] ++ (if system.isWindows
          then [ (hsPkgs.Win32) ]
          else [ (hsPkgs.unix) ]);
      };
      benchmarks = {
        "loading-benchmark" = {
          depends  = [
            (hsPkgs.random)
            (hsPkgs.directory)
            (hsPkgs.system-fileio)
            (hsPkgs.system-filepath)
            (hsPkgs.criterion)
            (hsPkgs.mtl)
            (hsPkgs.base)
            (hsPkgs.acid-state)
          ];
        };
      };
    };
  } // {
    src = pkgs.fetchgit {
      url = "https://github.com/parsonsmatt/acid-state";
      rev = "63ac55ae020655104936d8a90ccc6a939642cd0d";
      sha256 = "0a0j3wx0zycb8r4ng8i7s99868vd6q2q39m81wpl3n05sxmlgkfm";
    };
  }