pkgs: localLib: self: super: {
    mkDerivation = args: super.mkDerivation (args // optionalAttrs (localLib.isCardanoSL args.pname) {
      configureFlags = (args.configureFlags or []) ++ [ "--ghc-options=-O0" ];
    });
  }
