let
  # Allow overriding pinned nixpkgs for debugging purposes via cardano_pkgs
  fetchNixPkgs = let try = builtins.tryEval <cardano_pkgs>;
    in if try.success
    then builtins.trace "using host <cardano_pkgs>" try.value
    else import ./nix/fetch-nixpkgs.nix;

  # Function to import the pinned nixpkgs with necessary overlays,
  # applied to the given args.
  importPkgs = args: import fetchNixPkgs ({ overlays = [ (import ./nix/overlays/jemalloc.nix) ]; } // args);

  # Gets the value of an environment variable, with a default if it's
  # unset or empty.
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
            lib.hasPrefix ".ghc.environment" baseName ||
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
  inherit fetchNixPkgs importPkgs cleanSourceTree;
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
      relays = "relays.cardano-mainnet.iohk.io";
      confKey = "mainnet_full";
      private = false;
    };
    staging = {
      relays = "relays.awstest.iohkdev.io";
      confKey = "mainnet_dryrun_full";
      private = false;
    };
    testnet = {
      relays = "relays.cardano-testnet.iohkdev.io";
      confKey = "testnet_full";
      private = false;
    };
    demo = {
      confKey = "dev";
      relays = "127.0.0.1";
      private = true;
    };
  };

  # Generates an attrset for the three cardano-sl networks by applying
  # the given function to each environment.
  #
  # Example:
  #   forEnvironments ({ environment, confKey, ... }: "key for ${environment} is ${confKey}")
  #   => { demo = "key for demo is dev";
  #        mainnet = "key for mainnet is mainnet_full";
  #        staging = "key for staging is mainnet_dryrun_full";
  #        testnet = "key for testnet is testnet_full"; }
  forEnvironments = f: lib.mapAttrs
    (name: env: f (env // { environment = name; }))
    environments;

  runHaskell = name: hspkgs: deps: env: code: let
    ghc = hspkgs.ghcWithPackages deps;
    builtBinary = pkgs.runCommand "${name}-binary" { buildInputs = [ ghc ]; } ''
      mkdir -p $out/bin/
      ghc ${pkgs.writeText "${name}.hs" code} -o $out/bin/${name}
    '';
  in pkgs.runCommand name env ''
    ${builtBinary}/bin/$name
  '';

  # Overrides the lib.commitIdFromGitRepo function in nixpkgs
  commitIdFromGitRepo = import ./nix/commit-id.nix { inherit lib; };

  # Plucks all the cardano-sl packages from the haskellPackages set,
  # also adding -static variants which contain the gitrev.
  getCardanoPackages = justStaticExecutablesGitRev: haskellPackages:
    let
      cslPackages = lib.filterAttrs (name: drv: isCardanoSL name) haskellPackages;
      makeStatic = name: drv: lib.nameValuePair (name + "-static") (justStaticExecutablesGitRev drv);
    in
      cslPackages // lib.mapAttrs' makeStatic cslPackages;

})
