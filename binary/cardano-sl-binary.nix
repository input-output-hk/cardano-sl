{ mkDerivation, aeson, base, binary, bytestring, canonical-json
, cardano-sl-util, cardano-sl-util-test, cborg, cereal, containers
, cpphs, digest, formatting, generic-arbitrary, half, hashable
, hedgehog, hspec, lens, mtl, pretty-show, QuickCheck
, quickcheck-instances, micro-recursion-schemes, safe-exceptions
, safecopy, serokell-util, stdenv, tagged, template-haskell, text
, th-utilities, time-units, universum, unordered-containers, vector
}:
mkDerivation {
  pname = "cardano-sl-binary";
  version = "2.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base binary bytestring canonical-json cardano-sl-util cborg
    cereal containers digest formatting hashable lens micro-recursion-schemes
    safe-exceptions safecopy serokell-util tagged template-haskell text
    th-utilities time-units universum unordered-containers vector
  ];
  libraryToolDepends = [ cpphs ];
  testHaskellDepends = [
    base bytestring cardano-sl-util-test cborg cereal containers
    formatting generic-arbitrary half hedgehog hspec mtl pretty-show
    QuickCheck quickcheck-instances safecopy serokell-util tagged text
    time-units universum unordered-containers
  ];
  testToolDepends = [ cpphs ];
  description = "Cardano SL - binary serialization";
  license = stdenv.lib.licenses.mit;
}
