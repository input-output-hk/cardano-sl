#!/usr/bin/env bash

set -Eeuo pipefail

# This scripts runs cardano cluster together with integration tests
# TODO (akegalj): we would like to have this code in haskell
# Note that launching tmux local clusters might not be supported by our CI (waiting to be tested)
# Shuting down tmux cluster also should be tested.

# cardano/scripts directory
scripts=$(dirname "$0")/../..
sessionName="integration-tests-"`date +%H%M%S`
tmpSecrets=$scripts/../temp-secrets

tlsCACert=$scripts/../run/tls-files/ca.crt
tlsClientCert=$scripts/../run/tls-files/client.crt
tlsClientKey=$scripts/../run/tls-files/client.key

SECONDS=$1

# remove generated keys and shut down cardano cluster
cleanState()
{
    rm -rf $tmpSecrets
    tmux kill-session -t $sessionName
}
trap "cleanState" ERR

# clean db
echo "Cleaning db state..."
$scripts/clean/db.sh

# generate keys
# TODO (akegalj): use OS temp location instead
echo "Creating genesis keys..."
rm -rf $tmpSecrets
stack exec -- cardano-keygen --system-start 0 generate-keys-by-spec --genesis-out-dir $tmpSecrets

# run cluster
echo "Starting local cardano cluster..."
tmux new-session -s $sessionName -d "scripts/launch/demo-with-wallet-api.sh"

# wait until cluster is fully up and running
echo "Waiting $SECONDS seconds until local cluster is ready..."
sleep "$SECONDS"s

# import keys
echo "Importing poor HD keys/wallet..."

for i in {0..11}
do
    echo "Imporing key$i.sk ..."
    curl -X POST https://localhost:8090/api/wallets/keys \
      --cacert $tlsCACert \
      --cert $tlsClientCert \
      --key $tlsClientKey \
      -H 'cache-control: no-cache' \
      -H 'content-type: application/json' \
      -d "\"$tmpSecrets/generated-keys/poor/key$i.sk\""
done

FAILED=0

# run integration tests
echo "Launching cardano integration tests..."
stack exec -- cardano-integration-test --help
stack exec -- cardano-integration-test --tls-ca-cert $tlsCACert --tls-client-cert $tlsClientCert --tls-key $tlsClientKey || {
    echo "Shutting down cardano cluster... it did a fail"
    cleanState
    FAILED=1
}

# kill cluster
if [[ $FAILED == 0 ]]; then
    echo "Shutting down cardano cluster..."
    cleanState
else
    exit 1
fi
