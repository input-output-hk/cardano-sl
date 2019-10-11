# This create a builder script which generates new genesis data using
# cardano-keygen.  It creates an new configuration.yaml with the
# correct hash of the genesis data.
with import ./../../lib.nix;

{ stdenv, writeScript, writeScriptBin
, python3, jq, coreutils, gnused, haskell, haskellPackages
, cardano-sl, cardano-sl-tools

# System start time parameter (UNIX time), or 0 to use the current time.
, systemStart ? 0

# The configuration section to update with new genesis data.
, configurationKey ? "testnet_full"

# The configuration section to use for creating the genesis data.
, configurationKeyLaunch ? "testnet_launch"

# Corresponds to "richmen" setting in configuration.yaml
, numCoreNodes ? 7
}:

let
  yaml2json = haskell.lib.disableCabalFlag haskellPackages.yaml "no-exe";
  genesisTools = [ yaml2json jq python3 coreutils gnused cardano-sl-tools ];
  configSource = cardano-sl.src + "/configuration.yaml";

in
  writeScript "prepare-genesis" ''
    #!${stdenv.shell}
    set -euo pipefail

    export PATH="${makeBinPath genesisTools}"
    src="${configSource}"
    out=$1

    if [ -z "$out" ]; then
      echo "usage: $0 OUTDIR"
      exit 1
    fi

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
