#!/usr/bin/env bash

echo "Wait for 10 sec while gathering results..."
(stack exec bench-receiver -- --port 3000 > receiver.log) & (sleep 1; stack exec bench-sender -- --peer 127.0.0.1:3000 > sender.log)
echo "Analysing logs..."
stack exec bench-log-reader -- -i sender.log -i receiver.log
echo "Copying results to clipboard"
cat measures.csv | xclip -selection clipboard
