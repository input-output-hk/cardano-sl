{ rev                             # The Git revision of nixpkgs to fetch
, sha256                          # The SHA256 of the downloaded data
, sha256unpacked
, system ? builtins.currentSystem # This is overridable if necessary
}:

if 0 <= builtins.compareVersions builtins.nixVersion "1.12"
then
  builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    sha256 = sha256unpacked;
  }
else
  (rec {
    tarball = import <nix/fetchurl.nix> {
      url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
      inherit sha256;
    };

    builtin-paths = import <nix/config.nix>;

    script = builtins.toFile "nixpkgs-unpacker" ''
      "$coreutils/mkdir" "$out"
      cd "$out"
      "$gzip" --decompress < "$tarball" | "$tar" -x --strip-components=1
    '';

    nixpkgs = builtins.derivation {
      name = "nixpkgs-${builtins.substring 0 6 rev}";

      builder = builtins.storePath builtin-paths.shell;

      args = [ script ];

      inherit tarball system;

      tar       = builtins.storePath builtin-paths.tar;
      gzip      = builtins.storePath builtin-paths.gzip;
      coreutils = builtins.storePath builtin-paths.coreutils;
    };
  }).nixpkgs
