{ localLib ? import ./../../lib.nix
, stateDir ? localLib.maybeEnv "CARDANO_STATE_DIR" "./state-demo"
, config ? {}
, system ? builtins.currentSystem
, pkgs ? import localLib.fetchNixPkgs { inherit system config; }
, gitrev ? localLib.commitIdFromGitRepo ./../../.git
, systemStart ? 0
, configurationKey ? "testnet_full"
, configurationKeyLaunch ? "testnet_launch"
, numCoreNodes ? 7
}:

with localLib;

let
  iohkPkgs = import ./../.. { inherit config system pkgs gitrev; };

  python = pkgs.python3.withPackages (ps: [ ps.pyyaml ]);

  yaml2json = pkgs.writeScriptBin "yaml2json" ''
    #!${python}/bin/python
    import json
    import sys
    import yaml
    json.dump(yaml.load(open(sys.argv[1])), sys.stdout)
  '';

  genesisTools = with pkgs; [ yaml2json jq python3 coreutils gnused iohkPkgs.cardano-sl-tools ];
  configSource = iohkPkgs.cardano-sl.src + "/configuration.yaml";

in
  pkgs.writeScript "prepare-genesis" ''
    #!${pkgs.stdenv.shell}
    set -euo pipefail

    export PATH=${pkgs.lib.makeBinPath genesisTools}
    src=${configSource}
    out=$1

    mkdir -p $out
    sed -e 's/richmen:.*/richmen: ${toString numCoreNodes}/g' $src > $out/configuration-launch.yaml
    cd $out

    # Generate very big random integer. For actual networks this
    # should be kept secret in a safe place.
    python -c "import secrets; print(secrets.randbelow(2**256))" > seed.txt

    ${if systemStart != 0 then "echo ${toString systemStart}" else "date '+%s'"} > system-start.txt

    cardano-keygen --system-start $(cat system-start.txt) \
      --configuration-file configuration-launch.yaml \
      --configuration-key ${configurationKeyLaunch} \
      --configuration-seed $(cat seed.txt) \
      dump-genesis-data --path genesis.json

    genesis-hash genesis.json > hash.txt

    # create updated configuration.yaml
    sed $(yaml2json configuration-launch.yaml | jq -r ".${configurationKey}.core.genesis.src|@text \"-e s/\\(.file)/genesis.json/ -e s/\\(.hash)/$(cat hash.txt)/\"") configuration-launch.yaml > configuration.yaml

    cardano-keygen --system-start 0 \
      --configuration-file configuration.yaml \
      --configuration-key ${configurationKeyLaunch} \
      --configuration-seed $(cat seed.txt) \
      generate-keys-by-spec --genesis-out-dir ./genesis-keys
  ''
