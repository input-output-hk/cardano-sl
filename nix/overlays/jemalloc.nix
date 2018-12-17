self: super: {
  # jemalloc has a bug that caused cardano-sl-db to fail to link (via
  # rocksdb, which can use jemalloc).
  # https://github.com/jemalloc/jemalloc/issues/937
  # Using jemalloc 510 with the --disable-initial-exec-tls flag seems to
  # fix it.
  jemalloc = self.callPackage ../jemalloc/jemalloc510.nix {};
}
