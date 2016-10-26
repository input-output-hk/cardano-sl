#!/bin/sh

i=$1

if [[ $i -lt 10 ]]; then
  port="300$i"
else
  port="30$i"
fi

stack exec -- pos-node --db-path pos-db$i --rebuild-db --vss-genesis $i --spending-genesis $i --port $port --peer '127.0.0.1:2000/ABOtPlQMv123_4wzfgjAzvsT2LE='
