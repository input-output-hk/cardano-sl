#!/usr/bin/env sh

# This scripts runs cardano cluster together with integration tests
# TODO (akegalj): we would like to have this code in haskell
# Note that launching tmux local clusters might not be supported by our CI (waiting to be tested)
# Shuting down tmux cluster also should be tested.

# cardano/scripts directory
scripts=$(dirname "$0")/../..

# clean db
$scripts/clean/db.sh

# generate keys
# TODO (akegalj): use OS temp location instead
rm -rf $scripts/tmp-secrets
stack exec -- cardano-keygen --system-start 0 generate-keys-by-spec --genesis-out-dir tmp-secrets

# import keys
# TODO: we should import multiple keys for them to be useful as they contain little amount of money. Check does fakeavvm contains more money and if it does use that instead
 curl -X POST --insecure https://localhost:8090/api/wallets/keys -H 'cache-control: no-cache' -H 'content-type: application/json' -d '"$scripts/../tmp-secrets/generated-keys/poor/key0.sk"'

# run cluster
# TODO (akegalj): move to integration/Main.hs
# WALLET_DEBUG flag is needed so that wallet is launched without TLS. We could also use --no-tls, but this involves changes to the underlying script. See https://iohk.myjetbrains.com/youtrack/issue/CT-7#comment=93-17848
WALLET_DEBUG=1 $scripts/launch/demo-with-new-wallet-api.sh

# run integration tests
# TODO (akegalj): add tls files to default integration options
stack exec -- cardano-integration-test --help
echo $scripts/tls-files/server.crt
stack exec -- cardano-integration-test # --tls-pub-cert scripts/tls-files/server.crt --tls-priv-key scripts/tls-files/server.key

# kill cluster
# TODO (akegalj): move to integration/Main.hs
$scripts/launch/kill-demo.sh
