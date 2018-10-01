pkgs: localLib: self: super: with pkgs.lib; {
    mkDerivation = args: super.mkDerivation (args // optionalAttrs (localLib.isCardanoSL args.pname) {
      # Enables building but not running of benchmarks for all
      # cardano-sl packages when enableBenchmarks argument is true.
      doBenchmark = true;
      configureFlags = (args.configureFlags or []) ++ ["--enable-benchmarks"];
    } // optionalAttrs (localLib.isBenchmark args) {
      # Provide a dummy installPhase for benchmark packages.
      installPhase = "mkdir -p $out";
    });
  }
