#!/bin/bash

# Count number of transactions that ended up in a block in latest logs/

last=`ls logs/ | sort | tail -1`

jq -Mr '[.timestamp, .event.createdBlock.txs | length] | @csv' logs/$last/node*.json | grep -v ',0' | cut -f2 -d',' | awk '{s+=$1} END {print s}'
