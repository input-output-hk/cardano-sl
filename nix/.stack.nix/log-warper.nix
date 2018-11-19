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
      specVersion = "2.0";
      identifier = {
        name = "log-warper";
        version = "1.8.10.1";
      };
      license = "MIT";
      copyright = "2016-2018 Serokell";
      maintainer = "Serokell <hi@serokell.io>";
      author = "@serokell";
      homepage = "https://github.com/serokell/log-warper";
      url = "";
      synopsis = "Flexible, configurable, monadic and pretty logging";
      description = "This package implements nice and featureful wrapper around hslogger library.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends  = [
          (hsPkgs.base)
          (hsPkgs.aeson)
          (hsPkgs.ansi-terminal)
          (hsPkgs.containers)
          (hsPkgs.deepseq)
          (hsPkgs.directory)
          (hsPkgs.filepath)
          (hsPkgs.fmt)
          (hsPkgs.lifted-async)
          (hsPkgs.microlens-platform)
          (hsPkgs.monad-control)
          (hsPkgs.monad-loops)
          (hsPkgs.mmorph)
          (hsPkgs.mtl)
          (hsPkgs.o-clock)
          (hsPkgs.text)
          (hsPkgs.time)
          (hsPkgs.transformers)
          (hsPkgs.transformers-base)
          (hsPkgs.universum)
          (hsPkgs.unordered-containers)
          (hsPkgs.vector)
          (hsPkgs.yaml)
        ] ++ pkgs.lib.optional (!system.isWindows) (hsPkgs.unix);
      };
      tests = {
        "log-test" = {
          depends  = [
            (hsPkgs.async)
            (hsPkgs.base)
            (hsPkgs.data-default)
            (hsPkgs.directory)
            (hsPkgs.filepath)
            (hsPkgs.hspec)
            (hsPkgs.HUnit)
            (hsPkgs.microlens-mtl)
            (hsPkgs.log-warper)
            (hsPkgs.QuickCheck)
            (hsPkgs.universum)
            (hsPkgs.unordered-containers)
          ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover)
          ];
        };
      };
    };
  } // {
    src = pkgs.fetchgit {
      url = "https://github.com/input-output-hk/log-warper.git";
      rev = "0b7d4d48310f139d75829d31883aaa87ce53312c";
      sha256 = "0m9inw7m42yj0kn3x3ip5ipv72pr34l4744myzlidbpqa2wf4b7g";
    };
  }