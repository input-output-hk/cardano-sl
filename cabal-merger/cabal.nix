{ mkDerivation, array, base, base-compat, base-orphans, binary
, bytestring, containers, deepseq, Diff, directory, filepath
, integer-logarithms, mtl, optparse-applicative, parsec, pretty
, process, QuickCheck, stdenv, tagged, tar, tasty, tasty-golden
, tasty-hunit, tasty-quickcheck, text, time, transformers
, tree-diff, unix
}:
mkDerivation {
  pname = "Cabal";
  version = "2.2.0.0";
  sha256 = "26a9b178fd83751f3fa6977922c447b63ac837a5371878ad7e1253beddfb042f";
  revision = "1";
  editedCabalFile = "1fa2lvwj1b0yj06k8pb3smdhdyl94dxy9ac9jqmmj9cdv8msrb8x";
  libraryHaskellDepends = [
    array base binary bytestring containers deepseq directory filepath
    mtl parsec pretty process text time transformers unix
  ];
  testHaskellDepends = [
    array base base-compat base-orphans bytestring containers deepseq
    Diff directory filepath integer-logarithms optparse-applicative
    pretty process QuickCheck tagged tar tasty tasty-golden tasty-hunit
    tasty-quickcheck text tree-diff
  ];
  doCheck = false;
  homepage = "http://www.haskell.org/cabal/";
  description = "A framework for packaging Haskell software";
  license = stdenv.lib.licenses.bsd3;
}
