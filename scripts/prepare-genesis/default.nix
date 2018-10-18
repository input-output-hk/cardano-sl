with import ./../../lib.nix;

{ stdenv, writeScript, writeScriptBin
, jq, coreutils, gnused, haskell, haskellPackages

, cardano-sl, cardano-sl-tools

## Options
, stateDir ? localLib.maybeEnv "CARDANO_STATE_DIR" "./state-demo"
, systemStart ? 0
, configurationKey ? "testnet_full"
, configurationKeyLaunch ? "testnet_launch"
, numCoreNodes ? 7
}:


let
  yaml2json = haskell.lib.disableCabalFlag haskellPackages.yaml "no-exe";
  genesisTools = [ yaml2json jq coreutils gnused cardano-sl-tools ];
  configSource = cardano-sl.src + "/configuration.yaml";

in
  writeScript "prepare-genesis" ''
    #!${stdenv.shell}
    set -euo pipefail

    export PATH=${makeBinPath genesisTools}
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
