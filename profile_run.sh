echo "Cleanup stuff"

rm -rf cardano-node.*
rm -rf ../mainnet-db-profiling
rm -rf ../staging-db-profiling
rm -rf wallet-db*

echo "Running!"

export EVIL_BLOCKS=100000

if [ "$1" == "mainnet" ]
then
echo "Running MAINNET..."
stack exec cardano-node -- --topology=mainnet-topology.yaml \
    --configuration-key mainnet_full  --db-path ../mainnet-db-profiling \
    --tlscert ../tls-certs/server.crt --tlskey ../tls-certs/server.key \
    --log-config log-warning.yaml \
    --tlsca ../tls-certs/ca.crt --no-client-auth +RTS -p 2>&1
else

if [ "$1" == "mainnet-legacy" ]
then
echo "Running MAINNET WITH LEGACY WALLET..."
stack exec cardano-node -- --topology=mainnet-topology.yaml \
    --configuration-key mainnet_full  --db-path ../mainnet-db-profiling \
    --tlscert ../tls-certs/server.crt --tlskey ../tls-certs/server.key \
    --log-config log-warning.yaml --legacy-wallet \
    --tlsca ../tls-certs/ca.crt --no-client-auth +RTS -p 2>&1

else
echo "Running STAGING..."
stack exec cardano-node -- --topology=docs/network/example-topologies/mainnet-staging.yaml \
    --configuration-key mainnet_dryrun_full  --db-path ../staging-db-profiling \
    --tlscert ../tls-certs/server.crt --tlskey ../tls-certs/server.key \
    --log-config log-warning.yaml \
    --tlsca ../tls-certs/ca.crt --no-client-auth +RTS -p 2>&1
fi
fi
