#!/bin/bash

# dev-mode : allow to generate custom genesis block
# CONFIG=.. selects section from config file (core/constants.yaml)
# dev-custom-config needed to override config file section 

stack build --flag cardano-sl-core:dev-mode --flag cardano-sl-core:dev-custom-config --ghc-options=-DCONFIG=benchmark

# CONC=4 transaction generator spawns 4 threads 
#   all send about 2 transactions per second
# 3 : 3 nodes
# no_dht: no Kalemdia
#
# To modify the behaviour of the benchmark generator: demo.sh, line 104 
# 
# > tmux send-keys -t ${pane} "sleep 40s && $(bench_cmd $i "$dht_conf" "$stake_distr" "$system_start" 300 $CONC 500 neighbours)" C-m
#
#   300         number of seconds it runs for
#   $CONC       number of threads
#   500         number of msec wait after sending a transaction (per thread)
#   neighbours  send transaction to every neighbour
#               OR: send-random   pick random neighbour each time
#                   round-robin   select neighbours round-robin (per thread)
#
# node_cmd , bench_cmd defined in scripts/common_functions.sh

CONC=4 scripts/launch/demo.sh 3 no_dht

# transaction generator generates file tps-sent.csv
#
# slotDuration=20,sendMode=SendNeighbours,conc=4,startTime=1500971310756016,delay=500,cooldown=10
# time,txCount,txType
# 1500971330773618,96,submitted
# 1500971330773618,0,failed
# 1500971350774060,92,submitted
# 1500971350774060,0,failed
#
# listing for each slot how many transaction it sent and how many it failed to send 
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
