{ compiler, flags ? {}, hsPkgs, pkgconfPkgs, pkgs, system }:
let
    _flags = {} // flags;
    in {
      flags = _flags;
      package = {
        specVersion = "1.10";
        identifier = {
          name = "plutus-prototype";
          version = "0.1.0.0";
        };
        license = "MIT";
        copyright = "";
        maintainer = "darryl.mcadams@iohk.io";
        author = "Darryl McAdams";
        homepage = "iohk.io";
        url = "";
        synopsis = "Prototype of the Plutus language";
        description = "Prototype of the Plutus language";
        buildType = "Simple";
      };
      components = {
        plutus-prototype = {
          depends  = [
            hsPkgs.base
            hsPkgs.bifunctors
            hsPkgs.binary
            hsPkgs.bytestring
            hsPkgs.cardano-crypto
            hsPkgs.cryptonite
            hsPkgs.ed25519
            hsPkgs.either
            hsPkgs.filepath
            hsPkgs.lens
            hsPkgs.memory
            hsPkgs.mtl
            hsPkgs.operational
            hsPkgs.parsec
            hsPkgs.transformers
          ];
        };
      };
    } // {
      src = pkgs.fetchgit {
        url = "https://github.com/avieth/plutus-prototype";
        rev = "d094be301195fcd8ab864d793f114970426a4478";
        sha256 = null;
      };
    }