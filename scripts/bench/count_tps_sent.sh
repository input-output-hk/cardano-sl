#!/bin/sh

# Count number of blocks sent by last invocation of the runbench

grep "submitted" < tps-sent.csv | cut -f2 -d',' | awk '{s+=$1} END {print s}'
