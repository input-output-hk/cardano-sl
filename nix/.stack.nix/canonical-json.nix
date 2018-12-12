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
        name = "canonical-json";
        version = "0.5.0.1";
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
      "library" = {
        depends  = [
          (hsPkgs.base)
          (hsPkgs.bytestring)
          (hsPkgs.containers)
          (hsPkgs.parsec)
          (hsPkgs.pretty)
        ];
      };
    };
  } // {
    src = pkgs.fetchgit {
      url = "https://github.com/input-output-hk/canonical-json.git";
      rev = "f66762e4c4fe484ed109d28ad541fc55cc841c2c";
      sha256 = "0cqa4vnrj3wqqgzzg9v7p3ddrfsf5fx63dv2jnwnlfaczs007dkq";
    };
  }