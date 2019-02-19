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
        depends = [
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
    };
  } // {
    src = pkgs.fetchgit {
      url = "https://github.com/input-output-hk/log-warper.git";
      rev = "16246d4fbf16da7984f2a4b6c42f2ed5098182e4";
      sha256 = "11vw6h3lshhwrjbxni6z0jr6w9x2x338rv6p2b4b0rgr650pv2a9";
    };
  }