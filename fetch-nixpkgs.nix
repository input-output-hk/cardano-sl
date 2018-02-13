with rec {
  ifThenElse = { bool, thenValue, elseValue }: (
    if bool then thenValue else elseValue);
  system = builtins.currentSystem;
  spec = builtins.fromJSON (builtins.readFile ./nixpkgs-src.json);
};

ifThenElse {
  bool = (0 <= builtins.compareVersions builtins.nixVersion "1.12pre");

  # In Nix 1.12, we can just give a `sha256` to `builtins.fetchTarball`.
  thenValue = (
    builtins.fetchTarball {
      url = "https://github.com/${spec.owner}/${spec.repo}/archive/${spec.rev}.tar.gz";
      sha256 = spec.unpackedsha256;
    });

  # This hack should at least work for Nix 1.11
  elseValue = (
    (rec {
      tarball = import <nix/fetchurl.nix> {
        url = "https://github.com/${spec.owner}/${spec.repo}/archive/${spec.rev}.tar.gz";
        sha256 = spec.tarsha256;
      };

      builtin-paths = import <nix/config.nix>;

      script = builtins.toFile "nixpkgs-unpacker" ''
        "$coreutils/mkdir" "$out"
        cd "$out"
        "$gzip" --decompress < "$tarball" | "$tar" -x --strip-components=1
      '';

      nixpkgs = builtins.derivation {
        name = "nixpkgs-${builtins.substring 0 6 spec.rev}";

        builder = builtins.storePath builtin-paths.shell;

        args = [ script ];

        inherit tarball system;

        tar       = builtins.storePath builtin-paths.tar;
        gzip      = builtins.storePath builtin-paths.gzip;
        coreutils = builtins.storePath builtin-paths.coreutils;
      };
    }).nixpkgs);
}
