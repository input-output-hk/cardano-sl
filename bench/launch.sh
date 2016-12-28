#!/usr/bin/env bash

echo "Wait for 50 sec while gathering results..."
(stack exec bench-receiver -- --port 5000 -d 50 > receiver0.log) & (stack exec bench-receiver -- --port 5001 -d 50 > receiver1.log) & (stack exec bench-receiver -- --port 5002 -d 50 > receiver2.log) & (sleep 1; stack exec bench-sender -- --peer 127.0.0.1:5000 --peer 127.0.0.1:5001 --peer 127.0.0.1:5002 -r 90 -m 10800 -d 50 +RTS -N -RTS > sender.log)
echo "Analysing logs..."
stack exec bench-log-reader -- -i sender.log -i receiver0.log -i receiver1.log -i receiver2.log
echo "Copying results to clipboard"
cat measures.csv | xclip -selection clipboard
