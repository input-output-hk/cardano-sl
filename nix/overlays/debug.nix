pkgs: self: super: {
    mkDerivation = args: super.mkDerivation (args // {
      # TODO: DEVOPS-355
      dontStrip = true;
      configureFlags = (args.configureFlags or []) ++ [ "--ghc-options=-g --disable-executable-stripping --disable-library-stripping" "--profiling-detail=toplevel-functions"];
    });
  }
