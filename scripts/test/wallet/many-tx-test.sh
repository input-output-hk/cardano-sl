#!/usr/bin/env bash

# This script assumes that keys never changes.
# If they did, one have to update ids used in requests (variables below) accordingly.

keydir="./dev-keys"
key1="$keydir/1.key"
addrsNum=1000
acc1="Ae2tdPwUPEZEK5DvxPMtnTnUfQg8coWAAbNfLEvQ4GqWTe9h8d6AEkBDMce@2147483648"
acc2="Ae2tdPwUPEZMeiDfNHZ45V7RoaSqd4oSMuG4jo7asvmNHS193EEad1tUkeT@2147483648"
someAddr1="DdzFFzCqrhtB4QPDzHBCwbgLce3zMMBFf7ZzM8N1zN1vuaZjSLRuberakvfmKhtD9Eb7H3eEwLpsUpmihBzx4kagc13gdhYQJxp1hQXH"
money=100000000

echo -e "\nLooking for key files $key1"

echo -e "\nResetting wallet database..."
curl -k --cert scripts/tls-files/client.pem https://127.0.0.1:8090/api/test/reset -d "" &> /dev/null


echo -e "\nCreating 2 wallets..."

curl -k --request POST \
  --cert scripts/tls-files/client.pem \
  --url https://127.0.0.1:8090/api/wallets/keys \
  --header 'content-type: application/json' \
  --data "\"$key1\"" &> /dev/null

curl -k --request POST \
     --cert scripts/tls-files/client.pem \
     --url https://127.0.0.1:8090/api/wallets/new \
     --header 'content-type: application/json' \
     --data '{
  "cwInitMeta": {
    "cwName": "My lovely wallet",
    "cwAssurance": "CWANormal",
    "cwUnit": 0
  },
  "cwBackupPhrase": {
      "bpToList":
      ["squirrel","material","silly","twice","direct","slush","pistol","razor","become","junk","kingdom","flee"]
    }
}'


echo -e "\nCreating $addrsNum addresses on wallet #2..."
# single address creation is long, running in parallel
for i in `seq 1 $addrsNum`;
do
    curl -k --request POST \
      --cert scripts/tls-files/client.pem \
      --url https://127.0.0.1:8090/api/addresses/ \
      --header 'content-type: application/json' \
      --data "\"$acc2\"" &> /dev/null
done;


echo "Sending money to wallet #2..."
addrs2="$(curl -k --cert scripts/tls-files/client.pem https://localhost:8090/api/accounts/$acc2 | grep -o -E \"cadId\":\"[A-Za-z0-9]*\" | cut -c 9- | sed 's/\"//g' )"
for addr2 in $addrs2
do
    curl -k --request POST \
         --cert scripts/tls-files/client.pem \
         --url https://127.0.0.1:8090/api/txs/payments/$acc1/$addr2/$money \
         --header 'content-type: application/json' &> /dev/null
done;


echo -e "\nCreated. Press Enter to send final transaction..."
read -n 1

echo -e "\nMaking transaction #2 -> #1..."
curl -k --request POST \
     --cert scripts/tls-files/client.pem \
     --url https://127.0.0.1:8090/api/txs/payments/$acc2/$someAddr1/$(($money*2)) \
  --header 'content-type: application/json' &> /dev/null


echo -e "\nDone"
