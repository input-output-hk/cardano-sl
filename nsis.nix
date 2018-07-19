{ pkgs }:
let arch = "x86"; # "amd64" #x86" -- amd64, because we are using mingwW64
    nsis = { stdenv, lib, fetchurl, scons, gcc, zlib, pkgs }:
with stdenv.lib;
stdenv.mkDerivation rec {
  name = "nsis";
  version = "3.03";
  src = fetchurl {
    url = "https://downloads.sourceforge.net/project/nsis/NSIS%203/${version}/nsis-${version}-src.tar.bz2";
    sha256 = "abae7f4488bc6de7a4dd760d5f0e7cd3aad7747d4d7cd85786697c8991695eaa";
  };

  # how do I extract `localSystem` form the nixpkgs that is calling this?
  buildGcc = (import <nixpkgs> { localSystem.system = "x86_64-linux"; }).pkgs.gcc;

  nativeBuildInputs = [ scons ];

  # the NSIS SCons script expects to find the `.dll` in /lib.
  # it also expects include and lib to be together.
  zlibJoin = pkgs.buildPackages.buildEnv {
    name = "full-zlib";
    paths = [ zlib.dev zlib ];
    postBuild = ''
      cp $out/bin/*.dll $out/lib
    '';
  };

  sconsArgs = lib.concatStringsSep "\" \"" [
    "STRIP=0"
    "ZLIB_W32=${zlibJoin}"
    "TARGET_ARCH=${arch}"
    "PREFIX=$out"
    "SKIPUTILS=NSIS Menu,zip2exe"
    "VERSION=${version}"
    "CC=${buildGcc}/bin/gcc"
    "CXX=${buildGcc}/bin/g++"
    "VERBOSE=1"
    "XGCC_W32_PREFIX=${if arch == "x86" then "i686" else "x86_64"}-pc-mingw32-"
    "APPEND_CPPPATH=${pkgs.buildPackages.zlib.dev}/include"
    "APPEND_LIBPATH=${pkgs.buildPackages.zlib}/lib"
    "PATH=$PATH"
  ];

  # TODO: note: building zip2exe fails due to zlib linking issues.
  buildPhase = ''
    scons makensis "${sconsArgs}"
  '';
  installPhase = ''
    scons install "${sconsArgs}"
  '';

  meta = with stdenv.lib; {
    descripition = "System to create Windows installers";
    homepage = "https://nsis.sourceforge.io/";
  };
};
in (import <nixpkgs> { crossSystem = (import <nixpkgs/lib>).systems.examples."${if arch == "x86" then "mingw32" else "mingwW64"}";
                       localSystem.system = pkgs.system; }).callPackage nsis {}

