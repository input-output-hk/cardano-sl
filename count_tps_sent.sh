#!/bin/bash

# Count number of blocks sent by last invocation of the runbench

cat tps-sent.csv | grep "submitted" | cut -f2 -d',' | awk '{s+=$1} END {print s}'
