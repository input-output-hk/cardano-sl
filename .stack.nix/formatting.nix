{ compiler, flags ? {}, hsPkgs, pkgconfPkgs, pkgs, system }:
let
    _flags = {} // flags;
    in {
      flags = _flags;
      package = {
        specVersion = "1.8";
        identifier = {
          name = "formatting";
          version = "6.3.6";
        };
        license = "BSD-3-Clause";
        copyright = "2013 Chris Done, Shachaf Ben-Kiki, Martijn van Steenbergen, Mike Meyer, 2011 MailRank, Inc.";
        maintainer = "chrisdone@gmail.com";
        author = "Chris Done, Shachaf Ben-Kiki, Martijn van Steenbergen, Mike Meyer, Bryan O'Sullivan";
        homepage = "";
        url = "";
        synopsis = "Combinator-based type-safe formatting (like printf() or FORMAT)";
        description = "Combinator-based type-safe formatting (like printf() or FORMAT), modelled from the HoleyMonoids package.";
        buildType = "Simple";
      };
      components = {
        formatting = {
          depends  = [
            hsPkgs.base
            hsPkgs.text
            hsPkgs.time
            hsPkgs.old-locale
            hsPkgs.scientific
            hsPkgs.clock
            hsPkgs.array
            hsPkgs.ghc-prim
            hsPkgs.text
            hsPkgs.transformers
            hsPkgs.bytestring
            hsPkgs.integer-gmp
            hsPkgs.semigroups
          ];
        };
        tests = {
          formatting-test = {
            depends  = [
              hsPkgs.base
              hsPkgs.formatting
              hsPkgs.hspec
              hsPkgs.semigroups
            ];
          };
        };
      };
    } // {
      src = pkgs.fetchgit {
        url = "https://github.com/chrisdone/formatting.git";
        rev = "444bc8b1190f9c7f94af41b6f538071545392754";
        sha256 = null;
      };
    }