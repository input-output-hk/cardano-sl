{ mkDerivation, aeson, aeson-pretty, attoparsec, base
, base16-bytestring, bytestring, canonical-json, cardano-sl-util
, cereal, cpphs, cryptonite, directory, file-embed, filepath
, formatting, hedgehog, hspec, mtl, pretty-show, QuickCheck
, quickcheck-instances, safecopy, stdenv, template-haskell, text
, time-units, universum, unordered-containers
}:
mkDerivation {
  pname = "cardano-sl-util-test";
  version = "2.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson aeson-pretty attoparsec base base16-bytestring bytestring
    canonical-json cardano-sl-util cereal cryptonite directory
    file-embed filepath formatting hedgehog hspec mtl pretty-show
    QuickCheck quickcheck-instances safecopy template-haskell text
    time-units universum unordered-containers
  ];
  libraryToolDepends = [ cpphs ];
  description = "Cardano SL - general utilities (tests)";
  license = stdenv.lib.licenses.mit;
}
