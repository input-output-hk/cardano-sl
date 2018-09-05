{ localLib ? import ./../../../lib.nix
, config ? {}
, system ? builtins.currentSystem
, pkgs ? import localLib.fetchNixPkgs { inherit system config; }
, gitrev ? localLib.commitIdFromGitRepo ../../../.git
, stateDir ? localLib.maybeEnv "CARDANO_STATE_DIR" "./state-acceptance-test-${environment}"
, environment ? "mainnet"
, resume ? true
}:

with localLib;

let
  iohkPkgs = import ./../../.. { inherit config system pkgs gitrev; };

  cardanoDeps = with iohkPkgs; [ cardano-sl-tools cardano-sl-wallet-new ];
  demoClusterDeps = with pkgs; [ jq coreutils curl gnused openssl ];
  allDeps =  demoClusterDeps ++ cardanoDeps;

  wallet = pkgs.callPackage ../../launch/connect-to-cluster { inherit gitrev stateDir environment; };

in
  pkgs.writeScript "acceptance-tests-${environment}" ''
    #!${pkgs.stdenv.shell}
    export PATH=${pkgs.lib.makeBinPath allDeps}:$PATH
    # Set to 0 (passing) by default. Tests using this cluster can set this variable
    # to force the `stop_wallet` function to exit with a different code.
    EXIT_STATUS=0
    function stop_wallet {
      trap "" INT TERM
      echo "Received TERM!"
      echo Killing wallet pid $wallet_pid
      kill $wallet_pid
      wait
      echo "Exiting with code $EXIT_STATUS!"
      exit $EXIT_STATUS
    }

    ${optionalString (!resume) ''
      # Remove previous state
      rm -rf ${stateDir}
    ''}
    mkdir -p ${stateDir}/logs

    trap "stop_wallet" INT TERM

    ${utf8LocaleSetting}
    echo Launching wallet node: ${wallet}
    ${wallet} &> ${stateDir}/logs/wallet.log &
    wallet_pid=$!

    start_time=$(date +%s)

    # Query node info until synced
    counter=0
    perc=0
    while [[ $perc != 100 ]]
    do
      info=$(curl --silent --cacert ${stateDir}/tls/client/ca.crt --cert ${stateDir}/tls/client/client.pem https://${wallet.walletListen}/api/v1/node-info | jq .data)
      perc=$(jq .syncProgress.quantity <<< "$info")
      height=$(jq .blockchainHeight.quantity <<< "$info")
      local_height=$(jq .localBlockchainHeight.quantity <<< "$info")

      if [[ $perc == "100" ]]
      then
        echo "Blockchain synced: $perc%  $height blocks"
      else
        if [[ -z "$perc" ]]; then
          echo "$(date +"%H:%M:%S") Blockchain syncing..."
        else
          echo "$(date +"%H:%M:%S") Blockchain syncing: $perc%  $local_height/$height blocks"
        fi
        counter=$((counter + 1))
        sleep 10
      fi

      if ! kill -0 $wallet_pid; then
        echo "Wallet is no longer running -- exiting"
        exit 1
      fi
    done

    finish_time=$(date +%s)
    elapsed_time=$(($finish_time - $start_time))

    echo
    echo "Blockchain sync complete!"
    echo "Blockchain height: $height blocks"
    echo "Elapsed time: $elapsed_time seconds"
  ''
