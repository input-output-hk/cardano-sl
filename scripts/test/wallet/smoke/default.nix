{ localLib ? import ./../../../../lib.nix
, config ? {}
, numCoreNodes ? 4
, stateDir ? localLib.maybeEnv "CARDANO_STATE_DIR" "./state-demo"
, system ? builtins.currentSystem
, pkgs ? import localLib.fetchNixPkgs { inherit system config; }
, gitrev ? "123456" # Dummy git revision to prevent mass rebuilds
, ghcRuntimeArgs ? "-N2 -qg -A1m -I0 -T"
, additionalNodeArgs ? ""
, useStackBinaries ? false
}:

with localLib;

let
  stackExec = optionalString useStackBinaries "stack exec -- ";
  cardanoDeps = with iohkPkgs; [ cardano-sl-tools ];
  integrationTestDeps = with pkgs; [ gnugrep ];
  allDeps = integrationTestDeps ++ (optionals (!useStackBinaries ) cardanoDeps);
  demo-cluster = iohkPkgs.demoCluster.override {
    inherit gitrev numCoreNodes stateDir useStackBinaries;
    keepAlive = false;
  };
  iohkPkgs = import ./../../../.. { inherit config system pkgs gitrev; };
in pkgs.writeScript "smoke-tests" ''
  #!${pkgs.stdenv.shell}
  export PATH=${pkgs.lib.makeBinPath allDeps}:$PATH
  set -e
  source ${demo-cluster}
  HEIGHT=$(curl --silent --cacert ${stateDir}/tls/client/ca.crt --cert ${stateDir}/tls/client/client.pem https://localhost:8090/api/v1/node-info | jq .data.localBlockChainHeight.quantity)
  echo "Checking sync every 30 seconds for 5 minutes"
  for i in {1..10}
  do
    sleep 30
    echo "checking sync (loop count: $i)"
    OLDHEIGHT=$HEIGHT
    HEIGHT=$(curl --silent --cacert ${stateDir}/tls/client/ca.crt --cert ${stateDir}/tls/client/client.pem https://localhost:8090/api/v1/node-info | jq .data.localBlockchainHeight.quantity)
    if [[ $HEIGHT -le $OLDHEIGHT ]]
    then
      echo "Wallet stopped syncing! $OLDHEIGHT -> $HEIGHT Failing!"
      EXIT_STATUS=1
      break;
    else
      echo "Wallet is syncing: $OLDHEIGHT -> $HEIGHT"
      EXIT_STATUS=0
    fi
  done
  stop_cardano
''
