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
# TODO (akegalj): move to integration/Main.hs
# WALLET_DEBUG flag is needed so that wallet is launched without TLS. We could also use --no-tls, but this involves changes to the underlying script. See https://iohk.myjetbrains.com/youtrack/issue/CT-7#comment=93-17848
echo "Starting local cardano cluster..."
tmux new-session -s $sessionName -d "WALLET_DEBUG=1 scripts/launch/demo-with-wallet-api.sh"

# wait until cluster is fully up and running
echo "Waiting 140 seconds until local cluster is ready..."
sleep 40s

# import keys
# TODO: we should import multiple keys for them to be useful as they contain little amount of money. Check does fakeavvm contains more money and if it does use that instead
echo "Importing poor HD key/wallet..."
 curl -X POST http://localhost:8090/api/wallets/keys -H 'cache-control: no-cache' -H 'content-type: application/json' -d "\"$tmpSecrets/generated-keys/poor/key0.sk\""

# run integration tests
# TODO (akegalj): add tls files to default integration options
echo "Launching cardano integration tests..."
stack exec -- cardano-integration-test --help
stack exec -- cardano-integration-test # --tls-pub-cert scripts/tls-files/server.crt --tls-priv-key scripts/tls-files/server.key

# kill cluster
# TODO (akegalj): move to integration/Main.hs
echo "Shutting down cardano cluster..."
cleanState
