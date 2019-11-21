let
  sources = import ./nix/sources.nix;
  pkgs' = import sources.nixpkgs {};
  haskellNixJson = let
    src = sources."haskell.nix";
  in __toJSON {
    inherit (sources."haskell.nix") rev sha256;
    url = "https://github.com/${src.owner}/${src.repo}";
  };
  iohkNix = import sources.iohk-nix { haskellNixJsonOverride = pkgs'.writeText "haskell-nix.json" haskellNixJson; };
  pkgs = iohkNix.pkgs;
  lib = pkgs.lib;
  niv = (import sources.niv {}).niv;
  maybeEnv = env: default:
    let
      result = builtins.getEnv env;
    in if result != ""
       then result
       else default;

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
  inherit environments forEnvironments niv iohkNix;
  utf8LocaleSetting = ''
    export LC_ALL=en_GB.UTF-8
    export LANG=en_GB.UTF-8
  '';
}
