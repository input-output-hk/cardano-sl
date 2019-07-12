{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "cardano-report-server"; version = "0.5.10"; };
      license = "BSD-3-Clause";
      copyright = "2017-2018 IOHK";
      maintainer = "volhovm.cs@gmail.com";
      author = "Volkhov Mikhail";
      homepage = "https://github.com/input-output-hk/cardano-report-server";
      url = "";
      synopsis = "Reporting server for CSL";
      description = "Please see README.md";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.aeson)
          (hsPkgs.aeson-pretty)
          (hsPkgs.base)
          (hsPkgs.bytestring)
          (hsPkgs.case-insensitive)
          (hsPkgs.directory)
          (hsPkgs.exceptions)
          (hsPkgs.filelock)
          (hsPkgs.filepath)
          (hsPkgs.formatting)
          (hsPkgs.http-types)
          (hsPkgs.lens)
          (hsPkgs.lifted-base)
          (hsPkgs.log-warper)
          (hsPkgs.monad-control)
          (hsPkgs.mtl)
          (hsPkgs.network)
          (hsPkgs.optparse-applicative)
          (hsPkgs.parsec)
          (hsPkgs.random)
          (hsPkgs.text)
          (hsPkgs.time)
          (hsPkgs.transformers)
          (hsPkgs.universum)
          (hsPkgs.vector)
          (hsPkgs.wai)
          (hsPkgs.wai-extra)
          (hsPkgs.warp)
          (hsPkgs.wreq)
          (hsPkgs.lens-aeson)
          ];
        };
      exes = {
        "cardano-report-server" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.cardano-report-server)
            (hsPkgs.directory)
            (hsPkgs.filepath)
            (hsPkgs.http-types)
            (hsPkgs.log-warper)
            (hsPkgs.monad-control)
            (hsPkgs.mtl)
            (hsPkgs.optparse-applicative)
            (hsPkgs.parsec)
            (hsPkgs.random)
            (hsPkgs.universum)
            (hsPkgs.wai-extra)
            (hsPkgs.warp)
            ];
          };
        };
      tests = {
        "cardano-report-server-test" = {
          depends = [
            (hsPkgs.HUnit)
            (hsPkgs.QuickCheck)
            (hsPkgs.aeson)
            (hsPkgs.base)
            (hsPkgs.cardano-report-server)
            (hsPkgs.hspec)
            (hsPkgs.lens)
            (hsPkgs.quickcheck-text)
            (hsPkgs.text)
            (hsPkgs.time)
            (hsPkgs.transformers)
            (hsPkgs.universum)
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover or (pkgs.buildPackages.hspec-discover))
            ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-report-server.git";
      rev = "93f2246c54436e7f98cc363b4e0f8f1cb5e78717";
      sha256 = "04zsgrmnlyjymry6fsqnz692hdp89ykqb8jyxib8yklw101gdn3x";
      });
    }