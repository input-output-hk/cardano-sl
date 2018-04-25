{ compiler, flags ? {}, hsPkgs, pkgconfPkgs, pkgs, system }:
let
    _flags = {} // flags;
    in {
      flags = _flags;
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
        log-warper = {
          depends  = [
            hsPkgs.base
            hsPkgs.aeson
            hsPkgs.ansi-terminal
            hsPkgs.containers
            hsPkgs.deepseq
            hsPkgs.directory
            hsPkgs.filepath
            hsPkgs.fmt
            hsPkgs.lifted-async
            hsPkgs.microlens-platform
            hsPkgs.monad-control
            hsPkgs.monad-loops
            hsPkgs.mmorph
            hsPkgs.mtl
            hsPkgs.o-clock
            hsPkgs.text
            hsPkgs.time
            hsPkgs.transformers
            hsPkgs.transformers-base
            hsPkgs.universum
            hsPkgs.unordered-containers
            hsPkgs.vector
            hsPkgs.yaml
          ] ++ pkgs.lib.optional (!system.isWindows) hsPkgs.unix;
        };
        exes = {
          play-log = {
            depends  = [
              hsPkgs.log-warper
              hsPkgs.universum
              hsPkgs.microlens
              hsPkgs.monad-control
              hsPkgs.yaml
            ] ++ pkgs.lib.optional (compiler.isGhc && compiler.version.ge "8.2.2") hsPkgs.o-clock;
          };
          how-to = {
            depends  = [
              hsPkgs.base
              hsPkgs.log-warper
              hsPkgs.markdown-unlit
              hsPkgs.text
            ];
          };
          pure-how-to = {
            depends  = [
              hsPkgs.base
              hsPkgs.log-warper
              hsPkgs.markdown-unlit
              hsPkgs.mtl
              hsPkgs.text
            ];
          };
        };
        tests = {
          log-test = {
            depends  = [
              hsPkgs.async
              hsPkgs.base
              hsPkgs.data-default
              hsPkgs.directory
              hsPkgs.filepath
              hsPkgs.hspec
              hsPkgs.HUnit
              hsPkgs.microlens-mtl
              hsPkgs.log-warper
              hsPkgs.QuickCheck
              hsPkgs.universum
              hsPkgs.unordered-containers
            ];
          };
        };
      };
    } // {
      src = pkgs.fetchgit {
        url = "https://github.com/input-output-hk/log-warper.git";
        rev = "5271ab6c33541b8155ca203e714875974ec116be";
        sha256 = null;
      };
    }