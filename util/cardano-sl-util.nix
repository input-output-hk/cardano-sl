{ mkDerivation, aeson, aeson-pretty, async, auto-update, base
, bytestring, canonical-json, cborg, cereal, concurrent-extra
, containers, contravariant, cpphs, cryptonite, deepseq, directory
, ether, exceptions, file-embed, filepath, formatting, hashable
, hedgehog, hspec, katip, lens, lrucache, megaparsec, mmorph
, monad-control, mtl, optparse-applicative, parsec, pretty-show
, process, QuickCheck, quickcheck-instances, reflection, resourcet
, safe-exceptions, safecopy, serokell-util, stdenv, stm, tagged
, template-haskell, text, time, time-units, transformers
, transformers-base, transformers-lift, universum, unliftio-core
, unordered-containers, yaml
}:
mkDerivation {
  pname = "cardano-sl-util";
  version = "2.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson auto-update base canonical-json cborg cereal concurrent-extra
    containers contravariant cryptonite deepseq directory ether
    exceptions file-embed filepath formatting hashable katip lens
    lrucache megaparsec mmorph monad-control mtl optparse-applicative
    parsec process reflection resourcet safe-exceptions serokell-util
    stm tagged template-haskell text time time-units transformers
    transformers-base transformers-lift universum unliftio-core
    unordered-containers yaml
  ];
  libraryToolDepends = [ cpphs ];
  testHaskellDepends = [
    aeson aeson-pretty async base bytestring canonical-json cereal
    directory file-embed filepath formatting hedgehog hspec pretty-show
    QuickCheck quickcheck-instances safecopy stm template-haskell text
    time time-units universum unordered-containers
  ];
  testToolDepends = [ cpphs ];
  homepage = "https://github.com/input-output-hk/cardano-sl";
  description = "Cardano SL - general utilities";
  license = stdenv.lib.licenses.mit;
}
