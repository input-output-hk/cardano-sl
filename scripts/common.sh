#!/usr/bin/env bash

base_common="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

function find_binary {
	binpath=`find $base_common/../.stack-work/install -type d -name bin | sort | tail -n1`
	echo "$binpath"/$1
}

function find_build_binary {
    binpath=`find $base_common/../.stack-work/dist -type d -name build | sort | tail -n1`
    echo "$binpath"/$1/$1
}

function ensure_run {
  run_dir="$base_common/../run"
  mkdir -p "$run_dir"
}

LOGS_TIME=`date '+%F_%H%M%S'`

function ensure_logs {
  logs_dir="$base_common/../logs/$LOGS_TIME"

	mkdir -p "$logs_dir"

	logs=''
	if [[ "$DHT_LOG" != "" ]]; then
	  logs="$logs --dht-log $DHT_LOG"
	fi
	if [[ "$MAIN_LOG" != "" ]]; then
	  logs="$logs --main-log $MAIN_LOG"
	fi
	if [[ "$COMM_LOG" != "" ]]; then
	  logs="$logs --comm-log $COMM_LOG"
	fi
	if [[ "$SERVER_LOG" != "" ]]; then
	  logs="$logs --server-log $SERVER_LOG"
	fi
}

function get_port {
  local i=$1
  local i2=$i
  if [[ $i -lt 10 ]]; then
    i2="0$i"
  fi
  echo "30$i2"
}
function dht_key {
  local i=$1
  local i2=$i
  if [[ $i -lt 10 ]]; then
    i2="0$i"
  fi
  echo "MHdtsP-oPf7UWly"$i2"7QuXnLK5RD="
}

function peer_config {
  local j=$1
  echo -n " --peer 127.0.0.1:"`get_port $j`'/'`dht_key $j`
}

function dht_config {
    local i="$1"
    shift
    local j=0
    if [[ "$1" == "all" ]]; then
        n=$2
        while [[ $j -lt $n ]]; do
            peer_config $j
            j=$((j+1))
        done
        echo -n " --explicit-initial"
    else
      while [[ $# -gt 0 ]]; do
        peer_config $1
        shift
      done
    fi
    if [[ "$i" != "rand" ]]; then
      echo -n " --dht-key "`dht_key $i`
    fi
}

function node_cmd {
  local i=$1
  local time_lord=$2
  local dht_cmd=$3
  local st=''
  local reb=''

  ensure_logs
  ensure_run

  if [[ $time_lord != "" ]] && [[ $time_lord != 0 ]]; then
    time_lord=" --time-lord"
  fi
  if [[ $NO_REBUILD == "" ]]; then
    reb=" --rebuild-db "
  fi

  echo -n "$(find_binary cardano-node) --db-path $run_dir/node-db$i $reb --vss-genesis $i"

  $dht_cmd

  echo -n " --spending-genesis $i --port "`get_port $i`
  echo -n " $logs $time_lord | tee $logs_dir/node-$i.log "
  echo ''
}
