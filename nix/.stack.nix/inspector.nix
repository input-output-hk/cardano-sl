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
      rev = "c975f4329365f0379c04358138e616fb96fb0b79";
      sha256 = "12q1v7a8kcw7qi4lws4j3mvxwfkhni6zmp870kmnkgbgwvrax9gs";
    };
  }) // {
    cabal-generator = "hpack";
  }