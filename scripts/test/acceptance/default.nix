with import ../../../lib.nix;

{ stdenv, writeScript
, jq, coreutils, curl, gnused, openssl, time, haskellPackages

, connect
, cardano-sl-tools, cardano-wallet, cardano-sl-wallet-tool

## Parameters for the test script
, stateDir ? maybeEnv "CARDANO_STATE_DIR" "./state-acceptance-test-${environment}"
, environment ? "mainnet"
, resume ? true
}:

let
  cardanoDeps = [ cardano-sl-tools cardano-wallet cardano-sl-wallet-tool ];
  testRunnerDeps = [ jq coreutils curl gnused openssl time haskellPackages.hp2pretty ];
  allDeps =  testRunnerDeps ++ cardanoDeps;

  wallet = connect {
    inherit environment stateDir;

    # This will limit heap size to 1GB, along with the usual RTS options.
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

    sleep 1  # wait for certificates to be created

    function wallet-tool() {
      cardano-sl-wallet-tool --cacert ${stateDir}/tls/client/ca.crt --pem ${stateDir}/tls/client/client.pem ${wallet.walletListen} "$@"
    }

    function bomb() {
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
    }

    # Query node info until synced
    wallet-tool wait-for-sync --pid $wallet_pid --out sync-stats.json || bomb

    # Restore a wallet
    echo
    echo "Going to restore a wallet"
    backupPhrase="session ring phone arrange notice gap media olympic water road spider rate"
    walletID="2cWKMJemoBakcSaWXEpvRNiAsnsVaNFkyVHaxxPghSZizwVaLqpGebJwjYSG6q1f9sw5i"
    ${optionalString resume ''wallet-tool delete-wallet "$walletID" || true''}
    wallet-tool restore-wallet --backup-phrase "$backupPhrase" --name "Acceptance Wallet"
    echo

    wallet-tool wait-for-restore --pid $wallet_pid --out restore-stats.json || bomb

    hp2pretty cardano-node.hp
    sync-plot sync sync-stats.json sync-stats.png
    sync-plot restore restore-stats.json restore-stats.png

    if [ -n "$BUILDKITE" ]; then
      buildkite-agent artifact upload sync-stats.png
      buildkite-agent artifact upload restore-stats.png
      buildkite-agent artifact upload cardano-node.hp
      buildkite-agent artifact upload cardano-node.svg
      echo "+++ Heap profile"
      printf '\033]1338;url='"artifact://cardano-node.svg"';alt='"Heap profile"'\a\n'
      echo "+++ Charts for ${environment}"
      printf '\033]1338;url='"artifact://sync-stats.png"';alt='"Blockchain sync"'\a\n'
      printf '\033]1338;url='"artifact://restore-stats.png"';alt='"Wallet restore"'\a\n'
    fi
  ''
