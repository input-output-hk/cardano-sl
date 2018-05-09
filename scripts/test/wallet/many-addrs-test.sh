#!/usr/bin/env bash

# This script assumes that keys never changes.
# If they did, one have to update requests accordingly.

keydir="./dev-keys"
key1="$keydir/1.key"
key2="$keydir/2.key"
addrsNum=1000

echo -e "\nLooking for key files $key1 and $key2"

echo -e "\nResetting wallet database..."
curl -k --cert scripts/tls-files/client.pem https://127.0.0.1:8090/api/test/reset -d ""


echo -e "\nCreating 2 wallets..."
curl -k --request POST \
  --cert scripts/tls-files/client.pem \
  --url https://127.0.0.1:8090/api/wallets/keys \
  --header 'content-type: application/json' \
  --data "\"$key1\""

curl -k --request POST \
  --cert scripts/tls-files/client.pem \
  --url https://127.0.0.1:8090/api/wallets/keys \
  --header 'content-type: application/json' \
  --data "\"$key2\""


echo -e "\nCreating $addrsNum addresses on wallet #1..."
# single address creation is long, running in parallel
for i in `seq 1 $addrsNum`;
do
    curl -k --request POST \
      --cert scripts/tls-files/client.pem \
      --url https://127.0.0.1:8090/api/addresses/ \
      --header 'content-type: application/json' \
      --data '"Ae2tdPwUPEZEK5DvxPMtnTnUfQg8coWAAbNfLEvQ4GqWTe9h8d6AEkBDMce@2147483648"' &> /dev/null
done;


echo -e "\nCreated. Press Enter to send transaction..."
read -n 1

echo -e "\nMaking transaction #1 -> #2..."
curl -k --request POST \
  --cert scripts/tls-files/client.pem \
  --url https://127.0.0.1:8090/api/txs/payments/Ae2tdPwUPEZEK5DvxPMtnTnUfQg8coWAAbNfLEvQ4GqWTe9h8d6AEkBDMce@2147483648/DdzFFzCqrhst5SwMdCBNP63rdwMvwgq9hZMUaM3qJmTxMrdysPsQGWLNXFunbEUN4HrxfqnHsRE2ofZkh6o8bHf2Efo97rfpQS2dBgD2/100000 \
  --header 'content-type: application/json'


echo -e "\nDone"
