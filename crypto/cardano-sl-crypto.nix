{ mkDerivation, aeson, base, binary, bytestring, canonical-json
, cardano-crypto, cardano-sl-binary, cardano-sl-binary-test
, cardano-sl-util, cardano-sl-util-test, cborg, cereal, cpphs
, cryptonite, cryptonite-openssl, data-default, formatting
, generic-arbitrary, hashable, hedgehog, hspec, lens, memory, mtl
, pvss, QuickCheck, quickcheck-instances, reflection
, safe-exceptions, safecopy, scrypt, serokell-util, stdenv, text
, universum, unordered-containers
}:
mkDerivation {
  pname = "cardano-sl-crypto";
  version = "2.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base binary bytestring canonical-json cardano-crypto
    cardano-sl-binary cardano-sl-util cborg cereal cryptonite
    cryptonite-openssl data-default formatting hashable lens memory mtl
    pvss reflection safe-exceptions safecopy scrypt serokell-util text
    universum unordered-containers
  ];
  libraryToolDepends = [ cpphs ];
  testHaskellDepends = [
    base bytestring cardano-crypto cardano-sl-binary
    cardano-sl-binary-test cardano-sl-util cardano-sl-util-test
    cryptonite formatting generic-arbitrary hedgehog hspec memory
    QuickCheck quickcheck-instances text universum unordered-containers
  ];
  description = "Cardano SL - cryptography primitives";
  license = stdenv.lib.licenses.mit;
}
