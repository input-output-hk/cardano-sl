{ mkDerivation, base, bytestring, cardano-sl-binary
, cardano-sl-util-test, cborg, cereal, containers, cpphs
, formatting, half, hedgehog, hspec, mtl, pretty-show, QuickCheck
, quickcheck-instances, safecopy, serokell-util, stdenv, text
, universum
}:
mkDerivation {
  pname = "cardano-sl-binary-test";
  version = "2.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring cardano-sl-binary cardano-sl-util-test cborg cereal
    containers formatting half hedgehog hspec mtl pretty-show
    QuickCheck quickcheck-instances safecopy serokell-util text
    universum
  ];
  libraryToolDepends = [ cpphs ];
  description = "Cardano SL - binary serializarion (tests)";
  license = stdenv.lib.licenses.mit;
}
