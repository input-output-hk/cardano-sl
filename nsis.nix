{ stdenv, fetchurl, scons, zlib, pkgs
, arch ? "x86" # or "amd64" for 64bit
}:
stdenv.mkDerivation rec {
  name = "nsis";
  version = "3.03";
  src = fetchurl {
    url = "https://downloads.sourceforge.net/project/nsis/NSIS%203/${version}/nsis-${version}-src.tar.bz2";
    sha256 = "abae7f4488bc6de7a4dd760d5f0e7cd3aad7747d4d7cd85786697c8991695eaa";
  };

  nativeBuildInputs = [ scons ];

  buildInputs = [ zlib ];

  # the NSIS SCons script expects to find the `.dll` in /lib.
  # it also expects include and lib to be together.
  zlibJoin = pkgs.buildPackages.buildEnv {
    name = "full-zlib";
    paths = [ zlib.dev zlib ];
    postBuild = ''
      cp $out/bin/*.dll $out/lib
    '';
  };

  # TODO: note: building zip2exe fails due to zlib linking issues.
  buildPhase = ''
    scons "makensis" STRIP=0 "ZLIB_W32=${zlibJoin}" "TARGET_ARCH=${arch}" "SKIPUTILS=NSIS Menu,zip2exe" "VERSION=${version}"
  '';
  installPhase = ''
    scons install STRIP=0 "ZLIB_W32=${zlibJoin}" "TARGET_ARCH=${arch}" "SKIPUTILS=NSIS Menu,zip2exe" "VERSION=${version}" "PREFIX=$out"
  '';

  meta = with stdenv.lib; {
    descripition = "System to create Windows installers";
    homepage = "https://nsis.sourceforge.io/";
  };
}
