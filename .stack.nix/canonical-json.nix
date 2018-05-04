{ compiler, flags ? {}, hsPkgs, pkgconfPkgs, pkgs, system }:
let
    _flags = {} // flags;
    in {
      flags = _flags;
      package = {
        specVersion = "1.10";
        identifier = {
          name = "canonical-json";
          version = "0.5.0.0";
        };
        license = "BSD-3-Clause";
        copyright = "Copyright 2015-2017 Well-Typed LLP";
        maintainer = "duncan@well-typed.com, edsko@well-typed.com";
        author = "Duncan Coutts, Edsko de Vries";
        homepage = "";
        url = "";
        synopsis = "Canonical JSON for signing and hashing JSON values";
        description = "An implementation of Canonical JSON.\n\n<http://wiki.laptop.org/go/Canonical_JSON>\n\nThe \\\"canonical JSON\\\" format is designed to provide\nrepeatable hashes of JSON-encoded data. It is designed\nfor applications that need to hash, sign or authenitcate\nJSON data structures, including embedded signatures.\n\nCanonical JSON is parsable with any full JSON parser, and\nit allows whitespace for pretty-printed human readable\npresentation, but it can be put into a canonical form\nwhich then has a stable serialised representation and\nthus a stable hash.";
        buildType = "Simple";
      };
      components = {
        canonical-json = {
          depends  = [
            hsPkgs.base
            hsPkgs.bytestring
            hsPkgs.containers
            hsPkgs.parsec
            hsPkgs.pretty
          ];
        };
      };
    } // {
      src = pkgs.fetchgit {
        url = "https://github.com/angerman/canonical-json.git";
        rev = "37f8140af7b72c794c6293193433fd6dfab7b023";
        sha256 = null;
      };
    }