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
in lib // iohkNix // (rec {
  utf8LocaleSetting = ''
    export LC_ALL=en_GB.UTF-8
    export LANG=en_GB.UTF-8
  '';

  # Blockchain networks and their configuration keys
  environments = {
    mainnet = {
      relays = "relays.cardano-mainnet.iohk.io";
      confKey = "mainnet_full";
      genesisFile = ./lib/mainnet-genesis.json;
      genesisHash = "5f20df933584822601f9e3f8c024eb5eb252fe8cefb24d1317dc3d432e940ebb";
      private = false;
    };
    staging = {
      relays = "relays.awstest.iohkdev.io";
      confKey = "mainnet_dryrun_full";
      genesisFile = ./lib/mainnet-genesis-dryrun-with-stakeholders.json;
      genesisHash = "c6a004d3d178f600cd8caa10abbebe1549bef878f0665aea2903472d5abf7323";
      private = false;
    };
    testnet = {
      relays = "relays.cardano-testnet.iohkdev.io";
      confKey = "testnet_full";
      genesisFile = ./lib/testnet-genesis.json;
      genesisHash = "96fceff972c2c06bd3bb5243c39215333be6d56aaf4823073dca31afe5038471";
      private = false;
    };
    shelley_staging = {
      relays = "relays.shelley-staging.aws.iohkdev.io";
      confKey = "shelley_staging_full";
      genesisFile = ./lib/shelley-staging-genesis.json;
      genesisHash = "82995abf3e0e0f8ab9a6448875536a1cba305f3ddde18cd5ff54c32d7a5978c6";
      private = false;
    };
    shelley_staging_short = {
      relays = "relays.staging-shelley-short.aws.iohkdev.io";
      confKey = "shelley_staging_short_full";
      genesisFile = ./lib/shelley-staging-short-genesis.json;
      genesisHash = "752ad119c4eb4cda927acb6f9e234887794775a22e9c11fb967d5fc2705bab04";
      private = false;
      pbftThreshold = "0.9";
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

})
