{ system
, compiler
, flags
, pkgs
, hsPkgs
, pkgconfPkgs
, ... }:
  ({
    flags = {};
    package = {
      specVersion = "0";
      identifier = {
        name = "inspector";
        version = "0.2";
      };
      license = "BSD-3-Clause";
      copyright = "2017-2018 PrimeType Ltd";
      maintainer = "nicolas@primetype.co.uk";
      author = "Nicolas Di Prima";
      homepage = "https://github.com/primetype/inspector#readme";
      url = "";
      synopsis = "";
      description = "";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.basement)
          (hsPkgs.cryptonite)
          (hsPkgs.foundation)
          (hsPkgs.memory)
        ];
      };
      exes = {
        "example" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.basement)
            (hsPkgs.cryptonite)
            (hsPkgs.foundation)
            (hsPkgs.inspector)
            (hsPkgs.memory)
          ];
        };
      };
    };
  } // {
    src = pkgs.fetchgit {
      url = "https://github.com/primetype/inspector.git";
      rev = "964558881210bf1f9387b51ab05057b1290d1d71";
      sha256 = "1ig1gb131z37jbg5ih2lv609f4jgw9wmm6lcxdclihjq5lm12b7n";
    };
  }) // {
    cabal-generator = "hpack";
  }