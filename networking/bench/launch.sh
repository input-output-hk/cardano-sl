#!/usr/bin/env bash

echo "Wait for 50 sec while gathering results..."
(stack exec bench-receiver -- --logs-prefix log_rcvr1 --port 5000 -d 50 > receiver0.log) & (stack exec bench-receiver -- --logs-prefix log_rcvr2 --port 5001 -d 50 > receiver1.log) & (stack exec bench-receiver -- --logs-prefix log_rcvr3 --port 5002 -d 50 > receiver2.log) & (sleep 1; stack exec bench-sender -- --logs-prefix log_sndr --peer 127.0.0.1:5000 --peer 127.0.0.1:5001 --peer 127.0.0.1:5002 -r 90 -m 10800 -d 50 +RTS -N -RTS > sender.log)
echo "Analysing logs..."
stack exec bench-log-reader -- -i sender.log -i receiver0.log -i receiver1.log -i receiver2.log
echo "Copying results to clipboard"
xclip -selection clipboard < measures.csv
