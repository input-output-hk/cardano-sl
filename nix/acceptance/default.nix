with import ../../lib.nix;

{ stdenv, writeScript
, jq, coreutils, curl, gnused, openssl, time, haskellPackages
, connect

## Parameters for the test script
, stateDir ? maybeEnv "CARDANO_STATE_DIR" "./state-acceptance-test-${environment}"
, environment ? "mainnet"
, resume ? true
}:

let
  testRunnerDeps = [ jq coreutils curl gnused openssl time haskellPackages.hp2pretty ];
  allDeps = testRunnerDeps;

  wallet = connect {
    inherit environment stateDir;

    # This will limit heap size to 1GB, and add heap profiling, along
    # with the usual RTS options.
    ghcRuntimeArgs = "-N2 -qg -A1m -I0 -T -M1G -h";
  };

in
  writeScript "acceptance-tests-${environment}" ''
    #!${stdenv.shell}
    export PATH=${makeBinPath allDeps}:$PATH
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

    trap "stop_wallet" INT TERM EXIT

    ${utf8LocaleSetting}
    echo Launching wallet node: ${wallet}
    ${time}/bin/time -v ${wallet} &> ${stateDir}/logs/wallet.log &
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
        echo
        echo "***"
        echo "*** Here are the last 200 lines of ${stateDir}/logs/wallet.log"
        echo "***"
        tail -n200 ${stateDir}/logs/wallet.log
        echo
        echo "***"
        echo "*** Wallet is no longer running -- exiting"
        echo "***"
        exit 1
      fi
    done

    finish_time=$(date +%s)
    elapsed_time=$(($finish_time - $start_time))

    echo
    echo "Blockchain sync complete!"
    echo "Blockchain height: $height blocks"
    echo "Elapsed time: $elapsed_time seconds"

    # Restore a wallet
    echo
    echo "Going to restore a wallet"
    curl --silent --cacert ${stateDir}/tls/client/ca.crt --cert ${stateDir}/tls/client/client.pem -H "Content-Type: application/json" https://${wallet.walletListen}/api/v1/wallets \
      -d '{
      "operation": "restore",
      "backupPhrase": ["session", "ring", "phone", "arrange", "notice", "gap", "media", "olympic", "water", "road", "spider", "rate"],
      "assuranceLevel": "normal",
      "name": "Acceptance Wallet"
    }'
    echo

    start_time=$(date +%s)

    perc=0
    throughput=""
    syncState=""
    while [[ "$syncState" != '"synced"' ]]
    do
      info=$(curl --silent --cacert ${stateDir}/tls/client/ca.crt --cert ${stateDir}/tls/client/client.pem https://${wallet.walletListen}/api/v1/wallets | jq .data[0])
      syncState=$(jq .syncState.tag <<< "$info")
      perc=$(jq .syncState.data.percentage.quantity <<< "$info")
      throughput="$throughput $(jq .syncState.data.throughput.quantity <<< "$info")"

      if [[ -z "$perc" ]]; then
        echo "$(date +"%H:%M:%S") Wallet restoring..."
      else
        avg=$(tr ' ' '\n' <<< $throughput | awk '{sum+=$1} END {print sum/NR}')
        echo "$(date +"%H:%M:%S") Wallet restoring: $perc% ($avg blocks/s)"
      fi
      sleep 10
          if ! kill -0 $wallet_pid; then
            echo
            echo "***"
            echo "*** Here are the last 200 lines of ${stateDir}/logs/wallet.log"
            echo "***"
            tail -n200 ${stateDir}/logs/wallet.log
            echo
            echo "***"
            echo "*** Wallet is no longer running -- exiting"
            echo "***"
            exit 1
          fi
    done

    finish_time=$(date +%s)
    elapsed_time=$(($finish_time - $start_time))

    echo
    echo "Restoration complete!"
    echo "Elapsed time: $elapsed_time seconds"

    hp2pretty cardano-node.hp

    if [ -n "$BUILDKITE" ]; then
      buildkite-agent artifact upload cardano-node.svg
      printf '\033]1338;url='"artifact://cardano-node.svg"';alt='"Heap profile"'\a\n'
    fi

    ${optionalString (!resume) ''
      # Remove our state to make space for other jobs
      rm -rf ${stateDir}
    ''}
  ''
