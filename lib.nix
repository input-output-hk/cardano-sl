let
  # Allow overriding pinned nixpkgs for debugging purposes via cardano_pkgs
  # Imports the iohk-nix library.
  # The version can be overridden for debugging purposes by setting
  # NIX_PATH=iohk_nix=/path/to/iohk-nix
  iohkNix = import (
    let try = builtins.tryEval <iohk_nix>;
    in if try.success
    then builtins.trace "using host <iohk_nix>" try.value
    else
      let
        spec = builtins.fromJSON (builtins.readFile ./nix/iohk-nix-src.json);
      in builtins.fetchTarball {
        url = "${spec.url}/archive/${spec.rev}.tar.gz";
        inherit (spec) sha256;
      }) {};
  # Gets the value of an environment variable, with a default if it's
  # unset or empty.
  maybeEnv = env: default:
    let
      result = builtins.getEnv env;
    in if result != ""
       then result
       else default;

  pkgs = iohkNix.pkgs;
  lib = pkgs.lib;
  environments = iohkNix.cardanoLib.environments // {
    demo = {
        confKey = "dev";
        relays = "127.0.0.1";
        private = true;
    };
  };
  forEnvironments = f: lib.mapAttrs
    (name: env: f (env // { inherit name; }))
    environments;
in lib // iohkNix.cardanoLib // iohkNix // {
  inherit environments forEnvironments;
  utf8LocaleSetting = ''
    export LC_ALL=en_GB.UTF-8
    export LANG=en_GB.UTF-8
  '';
}
