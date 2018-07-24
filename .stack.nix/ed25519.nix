{ system
, compiler
, flags ? {}
, pkgs
, hsPkgs
, pkgconfPkgs }:
  let
    _flags = {
      test-properties = true;
      test-hlint = true;
      test-doctests = true;
      no-donna = true;
    } // flags;
  in {
    flags = _flags;
    package = {
      specVersion = "1.10";
      identifier = {
        name = "ed25519";
        version = "0.0.5.0";
      };
      license = "MIT";
      copyright = "Copyright (c) Austin Seipp 2013-2017";
      maintainer = "Austin Seipp <aseipp@pobox.com>";
      author = "Austin Seipp";
      homepage = "https://thoughtpolice.github.com/hs-ed25519";
      url = "";
      synopsis = "Ed25519 cryptographic signatures";
      description = "This package provides a simple, fast, self-contained copy of the\nEd25519 public-key signature system with a clean interface. It also\nincludes support for detached signatures, and thorough documentation\non the design and implementation, including usage guidelines.";
      buildType = "Simple";
    };
    components = {
      "ed25519" = {
        depends  = [
          (hsPkgs.ghc-prim)
          (hsPkgs.base)
          (hsPkgs.bytestring)
        ];
        libs = pkgs.lib.optional (system.isWindows) (pkgs.advapi32);
      };
      tests = {
        "properties" = {
          depends  = pkgs.lib.optionals (!(!_flags.test-properties)) [
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.QuickCheck)
            (hsPkgs.ed25519)
          ];
        };
        "hlint" = {
          depends  = pkgs.lib.optionals (!(!_flags.test-hlint)) [
            (hsPkgs.base)
            (hsPkgs.hlint)
            (hsPkgs.filemanip)
          ];
        };
        "doctests" = {
          depends  = pkgs.lib.optionals (!(!_flags.test-doctests)) [
            (hsPkgs.base)
            (hsPkgs.filepath)
            (hsPkgs.directory)
            (hsPkgs.doctest)
            (hsPkgs.filemanip)
            (hsPkgs.ed25519)
          ];
        };
      };
      benchmarks = {
        "bench" = {
          depends  = [
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.criterion)
            (hsPkgs.deepseq)
            (hsPkgs.ed25519)
          ];
        };
      };
    };
  } // {
    src = pkgs.fetchgit {
      url = "https://github.com/thoughtpolice/hs-ed25519";
      rev = "da4247b5b3420120e20451e6a252e2a2ca15b43c";
      sha256 = "0fah4vkmqdkjsdh3s3x27yfaif2fbdg6049xvp54b5mh50yvxkfq";
    };
  }