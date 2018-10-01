pkgs: self: super: {
    mkDerivation = args: super.mkDerivation (args // {
      enablePhaseMetrics = true;
    });
  }
