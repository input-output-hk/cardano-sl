pkgs: self: super: {
    mkDerivation = args: super.mkDerivation (args // {
      doCheck = false;
    });
  }
