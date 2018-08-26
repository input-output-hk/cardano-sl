let
  # Allow overriding pinned nixpkgs for debugging purposes via cardano_pkgs
  fetchNixPkgs = let try = builtins.tryEval <cardano_pkgs>;
    in if try.success
    then builtins.trace "using host <cardano_pkgs>" try.value
    else import ./fetch-nixpkgs.nix;

  maybeEnv = env: default:
    let
      result = builtins.getEnv env;
    in if result != ""
       then result
       else default;

  # Removes files within a Haskell source tree which won't change the
  # result of building the package.
  # This is so that cached build products can be used whenever possible.
  # It also applies the lib.cleanSource filter from nixpkgs which
  # removes VCS directories, emacs backup files, etc.
  cleanSourceTree = src:
    if (builtins.typeOf src) == "path"
      then lib.cleanSourceWith {
        filter = with pkgs.stdenv;
          name: type: let baseName = baseNameOf (toString name); in ! (
            # Filter out cabal build products.
            baseName == "dist" || baseName == "dist-newstyle" ||
            baseName == "cabal.project.local" ||
            # Filter out stack build products.
            lib.hasPrefix ".stack-work" baseName ||
            # Filter out files which are commonly edited but don't
            # affect the cabal build.
            lib.hasSuffix ".nix" baseName
          );
        src = lib.cleanSource src;
      } else src;

  pkgs = import fetchNixPkgs {};
  lib = pkgs.lib;
in lib // (rec {
  inherit fetchNixPkgs cleanSourceTree;
  isCardanoSL = lib.hasPrefix "cardano-sl";
  isBenchmark = args: !((args.isExecutable or false) || (args.isLibrary or true));

  # Insert this into builder scripts where programs require a UTF-8
  # locale to work.
  utf8LocaleSetting = ''
    export LC_ALL=en_GB.UTF-8
    export LANG=en_GB.UTF-8
  '';

  # Blockchain networks and their configuration keys
  environments = {
    mainnet = {
      attr = "mainnet";
      relays = "relays.cardano-mainnet.iohk.io";
      confKey = "mainnet_full";
      private = false;
    };
    mainnet-staging = {
      attr = "staging";
      relays = "relays.awstest.iohkdev.io";
      confKey = "mainnet_dryrun_full";
      private = false;
    };
    testnet = {
      attr = "testnet";
      relays = "relays.cardano-testnet.iohkdev.io";
      confKey = "testnet_full";
      private = false;
    };
    demo = {
      attr = "demo";
      confKey = "dev";
      relays = "127.0.0.1";
      private = true;
    };
  };

  # Generates an attrset for the three cardano-sl networks by applying
  # the given function to each environment.
  forEnvironments = f: lib.mapAttrs'
    (name: env: lib.nameValuePair env.attr (f (env // { environment = name; })))
    (lib.filterAttrs (name: env: !env.private) environments);
})
