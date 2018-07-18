{ stdenv, fetchurl, unzip }:
stdenv.mkDerivation {
  name = "rocksdb-prebuilt";
  src = fetchurl {
    url = "https://s3.eu-central-1.amazonaws.com/ci-static/serokell-rocksdb-haskell-325427fc709183c8fdf777ad5ea09f8d92bf8585.zip";
    sha256 = "11w6nbg39y7n3j7d5p4mvls3h5sbld71nx8yxxrckh8ak8yr6kwp";
  };
  nativeBuildInputs = [ unzip ];
  buildInputs = [ unzip ];

  unpackPhase = ''
    unzip $src
  '';
  dontBuild = true;
  installPhase = ''
    install -d $out/lib
    # dlls
    for dll in $(find . -name "*.dll"); do
        install -C -m 755 $dll $out/lib
    done
    # libs
    for lib in $(find . -name "*.lib"); do
        install -C -m 755 $lib $out/lib
    done
    # archives
    for archive in $(find . -name "*.a"); do
        install -C -m 755 $archive $out/lib
    done
  '';
}
