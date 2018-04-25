{ compiler, flags ? {}, hsPkgs, pkgconfPkgs, pkgs, system }:
let
    _flags = {} // flags;
    in {
      flags = _flags;
      package = {
        specVersion = "1.10";
        identifier = {
          name = "cardano-report-server";
          version = "0.4.9";
        };
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
        cardano-report-server = {
          depends  = [
            hsPkgs.aeson
            hsPkgs.aeson-pretty
            hsPkgs.base
            hsPkgs.bytestring
            hsPkgs.case-insensitive
            hsPkgs.directory
            hsPkgs.exceptions
            hsPkgs.filelock
            hsPkgs.filepath
            hsPkgs.formatting
            hsPkgs.http-types
            hsPkgs.lens
            hsPkgs.lifted-base
            hsPkgs.log-warper
            hsPkgs.monad-control
            hsPkgs.mtl
            hsPkgs.network
            hsPkgs.optparse-applicative
            hsPkgs.parsec
            hsPkgs.random
            hsPkgs.text
            hsPkgs.time
            hsPkgs.transformers
            hsPkgs.universum
            hsPkgs.vector
            hsPkgs.wai
            hsPkgs.wai-extra
            hsPkgs.warp
            hsPkgs.wreq
            hsPkgs.lens-aeson
          ];
        };
        exes = {
          cardano-report-server = {
            depends  = [
              hsPkgs.base
              hsPkgs.cardano-report-server
              hsPkgs.directory
              hsPkgs.filepath
              hsPkgs.http-types
              hsPkgs.log-warper
              hsPkgs.monad-control
              hsPkgs.mtl
              hsPkgs.optparse-applicative
              hsPkgs.parsec
              hsPkgs.random
              hsPkgs.universum
              hsPkgs.wai-extra
              hsPkgs.warp
            ];
          };
        };
        tests = {
          cardano-report-server-test = {
            depends  = [
              hsPkgs.HUnit
              hsPkgs.QuickCheck
              hsPkgs.aeson
              hsPkgs.base
              hsPkgs.cardano-report-server
              hsPkgs.hspec
              hsPkgs.lens
              hsPkgs.quickcheck-text
              hsPkgs.text
              hsPkgs.time
              hsPkgs.transformers
              hsPkgs.universum
            ];
          };
        };
      };
    } // {
      src = pkgs.fetchgit {
        url = "https://github.com/input-output-hk/cardano-report-server.git";
        rev = "19fd5ddf24dab731375a156ce38dcfce4319d1e4";
        sha256 = null;
      };
    }