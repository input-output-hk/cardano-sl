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
        name = "iohk-monitoring";
        version = "0.1.2.0";
      };
      license = "MIT";
      copyright = "2018 IOHK";
      maintainer = "";
      author = "Alexander Diemand, Andreas Triantafyllos";
      homepage = "";
      url = "";
      synopsis = "logging, benchmarking and monitoring framework";
      description = "";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.aeson)
          (hsPkgs.array)
          (hsPkgs.async)
          (hsPkgs.attoparsec)
          (hsPkgs.auto-update)
          (hsPkgs.bytestring)
          (hsPkgs.clock)
          (hsPkgs.containers)
          (hsPkgs.contravariant)
          (hsPkgs.directory)
          (hsPkgs.ekg)
          (hsPkgs.ekg-core)
          (hsPkgs.filepath)
          (hsPkgs.katip)
          (hsPkgs.lens)
          (hsPkgs.mtl)
          (hsPkgs.safe-exceptions)
          (hsPkgs.scientific)
          (hsPkgs.stm)
          (hsPkgs.template-haskell)
          (hsPkgs.text)
          (hsPkgs.time)
          (hsPkgs.time-units)
          (hsPkgs.transformers)
          (hsPkgs.unordered-containers)
          (hsPkgs.vector)
          (hsPkgs.yaml)
          (hsPkgs.libyaml)
        ] ++ (if system.isWindows
          then [ (hsPkgs.Win32) ]
          else [ (hsPkgs.unix) ]);
      };
      exes = {
        "example-simple" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.iohk-monitoring)
            (hsPkgs.async)
            (hsPkgs.bytestring)
            (hsPkgs.mtl)
          ] ++ (if system.isWindows
            then [ (hsPkgs.Win32) ]
            else [ (hsPkgs.unix) ]);
        };
        "example-complex" = {
          depends = ([
            (hsPkgs.base)
            (hsPkgs.iohk-monitoring)
            (hsPkgs.async)
            (hsPkgs.bytestring)
            (hsPkgs.mtl)
            (hsPkgs.random)
            (hsPkgs.text)
          ] ++ (if system.isWindows
            then [ (hsPkgs.Win32) ]
            else [
              (hsPkgs.unix)
            ])) ++ pkgs.lib.optional (system.isLinux) (hsPkgs.download);
        };
      };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.iohk-monitoring)
            (hsPkgs.aeson)
            (hsPkgs.array)
            (hsPkgs.async)
            (hsPkgs.bytestring)
            (hsPkgs.clock)
            (hsPkgs.containers)
            (hsPkgs.directory)
            (hsPkgs.filepath)
            (hsPkgs.mtl)
            (hsPkgs.process)
            (hsPkgs.QuickCheck)
            (hsPkgs.random)
            (hsPkgs.semigroups)
            (hsPkgs.split)
            (hsPkgs.stm)
            (hsPkgs.tasty)
            (hsPkgs.tasty-hunit)
            (hsPkgs.tasty-quickcheck)
            (hsPkgs.text)
            (hsPkgs.time)
            (hsPkgs.time-units)
            (hsPkgs.transformers)
            (hsPkgs.unordered-containers)
            (hsPkgs.vector)
            (hsPkgs.void)
            (hsPkgs.yaml)
            (hsPkgs.libyaml)
          ];
        };
      };
    };
  } // {
    src = pkgs.fetchgit {
      url = "https://github.com/input-output-hk/iohk-monitoring-framework.git";
      rev = "aa430c860445817dc074ddb42ed29ad9e8da9f78";
      sha256 = "069bc110ipzv2xsn45m1fpl1lqb3219cmn1gfmid6qqc7rkv8vii";
    };
  }