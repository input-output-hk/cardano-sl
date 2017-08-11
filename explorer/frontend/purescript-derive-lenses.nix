{ mkDerivation, array, base, boxes, fetchgit, optparse-generic
, purescript, split, stdenv, text
}:
mkDerivation {
  pname = "purescript-derive-lenses";
  version = "0.10.5.0";
  src = fetchgit {
    url = "https://github.com/paf31/purescript-derive-lenses.git";
    sha256 = "0p53kdw1bcqpl0sls4c7ds0zf9ks5ivdgv23vbhdfcfl0bf107na";
    rev = "02457e610789263326b936ebdfa72edbb6599094";
  };
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    array base boxes optparse-generic purescript split text
  ];
  description = "A tool to derive lenses for PureScript data types";
  license = stdenv.lib.licenses.mit;
}
