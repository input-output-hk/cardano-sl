{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "canonical-json"; version = "0.6.0.0"; };
      license = "BSD-3-Clause";
      copyright = "Copyright 2015-2017 Well-Typed LLP";
      maintainer = "duncan@well-typed.com, edsko@well-typed.com";
      author = "Duncan Coutts, Edsko de Vries";
      homepage = "https://github.com/well-typed/canonical-json";
      url = "";
      synopsis = "Canonical JSON for signing and hashing JSON values";
      description = "An implementation of Canonical JSON.\n\n<http://wiki.laptop.org/go/Canonical_JSON>\n\nThe \\\"canonical JSON\\\" format is designed to provide\nrepeatable hashes of JSON-encoded data. It is designed\nfor applications that need to hash, sign or authenitcate\nJSON data structures, including embedded signatures.\n\nCanonical JSON is parsable with any full JSON parser, and\nit allows whitespace for pretty-printed human readable\npresentation, but it can be put into a canonical form\nwhich then has a stable serialised representation and\nthus a stable hash.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.bytestring)
          (hsPkgs.containers)
          (hsPkgs.deepseq)
          (hsPkgs.parsec)
          (hsPkgs.pretty)
          ];
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.canonical-json)
            (hsPkgs.containers)
            (hsPkgs.aeson)
            (hsPkgs.vector)
            (hsPkgs.unordered-containers)
            (hsPkgs.QuickCheck)
            (hsPkgs.tasty)
            (hsPkgs.tasty-quickcheck)
            ];
          };
        };
      benchmarks = {
        "parse-bench" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.canonical-json)
            (hsPkgs.containers)
            (hsPkgs.criterion)
            ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/well-typed/canonical-json.git";
      rev = "ddfe3593b80b5ceb88842bb7a6f2268df75d2c2f";
      sha256 = "02fzn1xskis1lc1pkz0j92v6ipd89ww0k2p3dvwpm3yap5dpnm7k";
      });
    }