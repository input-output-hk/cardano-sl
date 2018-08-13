#!/bin/sh

# CONC=4 transaction generator spawns 4 threads
#   all send about 2 transactions per second
#
# To modify the behaviour of the benchmark generator: demo.sh, line 104
#
# > tmux send-keys -t ${pane} "sleep 40s && $(bench_cmd $i "$stake_distr" "$system_start" 300 $CONC 500 neighbours)" C-m
#
#   300         number of transactions to send per thread
#   $CONC       number of threads
#   500         number of msec wait after sending a transaction (per thread)
#   neighbours  send transaction to every neighbour
#               OR: send-random   pick random neighbour each time
#                   round-robin   select neighbours round-robin (per thread)
#
# node_cmd , bench_cmd defined in scripts/common_functions.sh

# nodes wait for three minutes before creating blocks
system_start=$(($(date +%s) + 60*3)) RICH_NODES=3 CONC=4 NUM_TXS="$1" CONFIG_KEY="smallbench" scripts/launch/demo.sh 6 "$(dirname "$0")/topology" rich_poor

# transaction generator generates file tps-sent.csv
#
# slotDuration=20,sendMode=SendNeighbours,conc=4,startTime=1500971310756016,delay=500,cooldown=10
# time,txCount,txType
# 1500971330773618,96,submitted
# 1500971330773618,0,failed
# 1500971350774060,92,submitted
# 1500971350774060,0,failed
#
# listing for each slot how many transactions it sent and how many it failed to send
#
# generates a directory in logs/ directory. node0.json contains "events" from node0:
#
# {
#   "event": {
#     "createdBlock": {
#       "hash": "44e28bbe",
#       "prevBlock": "6dbfa75c",
#       "slot": [
#         0,   // epoch 0, slot 4
#         4
#       ],
#       "txs": [
#         "31a9f86ccea465268401b2f148cb1676c0240287c0417281b848bc02954a7a32",
#         ...
#       ]
#     }
#   },
#   "timestamp": 1500971352692605
# }
