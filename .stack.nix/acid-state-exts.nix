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
        name = "acid-state-exts";
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
      "acid-state-exts" = {
        depends  = [
          (hsPkgs.array)
          (hsPkgs.acid-state)
          (hsPkgs.base)
          (hsPkgs.bytestring)
          (hsPkgs.cereal)
          (hsPkgs.containers)
          (hsPkgs.directory)
          (hsPkgs.filelock)
          (hsPkgs.filepath)
          (hsPkgs.exceptions)
          (hsPkgs.extensible-exceptions)
          (hsPkgs.extra)
          (hsPkgs.hashable)
          (hsPkgs.mtl)
          (hsPkgs.network)
          (hsPkgs.safecopy)
          (hsPkgs.stm)
          (hsPkgs.template-haskell)
          (hsPkgs.th-expand-syns)
          (hsPkgs.time-units)
          (hsPkgs.unordered-containers)
        ] ++ (if system.isWindows
          then [ (hsPkgs.Win32) ]
          else [ (hsPkgs.unix) ]);
      };
    };
  } // rec {
    src = ../acid-state-exts;
  }