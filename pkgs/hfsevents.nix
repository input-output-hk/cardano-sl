{ mkDerivation, base, bytestring, cereal, Cocoa, CoreServices
, fetchgit, mtl, stdenv, text, unix
}:
mkDerivation {
  pname = "hfsevents";
  version = "0.1.6";
  src = fetchgit {
    url = "https://github.com/luite/hfsevents.git";
    sha256 = "0smpq3yd5m9jd9fpanaqvhadv6qcyp9y5bz0dya0rnxqg909m973";
    rev = "25a53d417d7c7a8fc3116b63e3ba14ca7c8f188f";
  };
  libraryHaskellDepends = [ base bytestring cereal mtl text unix ];
  librarySystemDepends = [ Cocoa ];
  libraryToolDepends = [ CoreServices ];
  homepage = "http://github.com/luite/hfsevents";
  description = "File/folder watching for OS X";
  license = stdenv.lib.licenses.bsd3;
  platforms = [ "x86_64-darwin" ];
}
