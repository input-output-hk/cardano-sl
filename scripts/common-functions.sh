#!/usr/bin/env bash

base_common="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

function find_binary {
  local nix=""
  [[ -z $2 ]] || nix="--nix"
  pushd "$base_common/.." > /dev/null || exit
  binpath=$(stack ${nix} path --local-install-root)/bin
  popd > /dev/null || exit
  echo "$binpath/$1"
}

function find_build_binary {
  local nix=""
  [[ -z $2 ]] || nix="--nix"
  pushd "$base_common/.." > /dev/null || exit
  binpath=$(stack ${nix} path --dist-dir)/build
  popd > /dev/null || exit
  echo "$binpath/$1/$1"
}

function ensure_run {
  if [[ "$1" == "" ]]
  then
    local run_dir="$base_common/../run"
  else
    run_dir="$1"
  fi
  mkdir -p "$run_dir"
}

LOGS_TIME=$(date '+%F_%H%M%S')

function ensure_logs {
  if [[ $1 == "" ]]
  then
    logs_dir="$base_common/../logs/$LOGS_TIME"
  else
    logs_dir=$1
  fi
  mkdir -p "${logs_dir}/logs"
}

function dump_path {
    ensure_logs
    echo -n "$logs_dir/dump/$1"
}

function logs {
  local log_file=$2
  ensure_logs "$1"

  local conf_dir="$logs_dir/conf"
  local template_name="../log-configs/template-demo.yaml"
  if [[ "$LOG_TEMPLATE" != "" ]]; then
    local template="$LOG_TEMPLATE"
  else
    local template_name="../log-configs/template-demo.yaml"
    local template="$base_common/$template_name"
  fi

  mkdir -p "$conf_dir"
  mkdir -p "$logs_dir/dump"

  local conf_file="$conf_dir/$log_file.yaml"
  # log files are named under the $logs_dir directory
  # sed "s|tmpFileName|$logs_dir/$log_file|g" "$template" > "$conf_file"
  sed "s|tmpFileName|$log_file|g" "$template" > "$conf_file"
  echo -n " --json-log=$logs_dir/logs/node$i.json "
  echo -n " --logs-prefix $logs_dir/logs --log-config $conf_file "
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

  $(find_binary cardano-dht-keygen) -n "000000000000$i2" | tr -d '\n'
}

# Generates kademliaN.yaml and topologyN.yaml for demo setup
# for N \in {0..n-1} where n is number of nodes specified.
function gen_kademlia_topology {
  local total_nodes=$1
  local npred=$((total_nodes-1))

  if [[ $total_nodes -le 0 ]]; then
    echo "gen_kademlia_topology: n should be positive, but is $total_nodes"
    exit
  fi

  if [[ "$2" == "" ]]; then
    local out_dir="./run/"
  else
    local out_dir="$2"
  fi

  echo "Cleaning up topology/kademlia files"
  rm -fv "$out_dir/topology*.yaml"
  rm -fv "$out_dir/kademlia*.yaml"

  echo "Generating new topology/kademlia files"
  for ((i=0; i<=total_nodes; i++)); do

    # generate kademlia config

    if [[ "$i" -eq "$total_nodes" ]]; then
      local kfile="$out_dir/kademlia_explorer.yaml"
      local others_n="$total_nodes"
    else
      local kfile="$out_dir/kademlia$i.yaml"
      local others_n="$npred"
    fi

    touch "$kfile"
    if [[ "$total_nodes" -eq 1 ]]; then
      echo "peers: []" > "$kfile"
    else
      echo "peers: " > "$kfile"
    fi

    for ((j=0; j<=others_n; j++)); do
      if [[ $j -eq $i ]]; then continue; fi
      {
      echo "  - host: '127.0.0.1'"
      echo "    port: 300$j"
      } >> "$kfile"
    done
    {
    echo "address:"
    echo "  host: '127.0.0.1'"
    echo "  port: 300$i"
    } >> "$kfile"


    # generate topology config (it's the same for all nodes)

    # we generate n-1 topology configs, there's no explorer topology (should there be?)
    if [[ $i -eq $total_nodes ]]; then continue; fi
    local tfile="$out_dir/topology$i.yaml"
    echo "nodes: " > "$tfile"
    for ((j=0; j<=npred; j++)); do

      local routes="["
      for ((k=0; k<=npred; k++)); do
        if [ "$k" -eq "$j" ]; then continue; fi
        routes="${routes[\"node$k\"]}, "
      done
      # If we have explorer add it so that the other nodes converse with it.
      routes="${routes[\"explorer\"]}]"
      {
      echo "  \"node$j\":"
      echo "    type: core"
      echo "    region: undefined"
      echo "    static-routes: $routes"
      echo "    addr: 127.0.0.1"
      echo "    port: 300$j"
      } >> "$tfile"

      # add explorer (as relay node)
      if [[ $j -eq $npred ]]; then
        # count port
        local exp=$((j + 1))
        # explorers routes
        local exr="["
        for ((k=0; k<=npred; k++)); do
          exr="${exr[\"node$k\"]}"
          # don't put comma after last element and after pre-last element of last list item
          if ! [ "$k" -eq "$npred" ]; then
            exr=$exr", "
          fi
        done
        exr="$exr]"

        {
          echo '  "explorer":'
          echo "    type: relay"
          echo "    region: undefined"
          echo "    static-routes: $exr"
          echo "    addr: 127.0.0.1"
          echo "    port: 300${exp[0]}"
        } >> "$tfile"
      fi
    done
  done

}

function node_cmd {
  local i=$1
  local wallet_args=$2
  local system_start=$3
  local config_dir=$4
  local conf_file=$5
  local log_dir=$6
  local run_dir=$7
  local reb=''
  local web=''
  local configuration=''

  ensure_run "$run_dir"

  keys_args="--genesis-secret $i"
  if [[ "$CSL_PRODUCTION" != "" ]]; then
      keys_args="--keyfile \"secrets/secret-$((i+1)).key\""
  elif [[ "$DEMO_GENESIS_KEYS_DIR" != "" ]]; then
      keys_args="--keyfile \"$DEMO_GENESIS_KEYS_DIR/key$i.sk\""
  fi

  if [[ $NO_REBUILD == "" ]]; then
    reb=" --rebuild-db "
  fi
  #shellcheck disable=SC2153
  if [[ "$REPORT_SERVER" != "" ]]; then
    report_server=" --report-server $REPORT_SERVER "
  fi
  if [[ "$CSL_RTS" != "" ]] && [[ $i -eq 0 ]]; then
    rts_opts="+RTS -N -pa -A6G -qg -RTS"
  fi


  if [[ "$conf_file" != "" ]]; then
     configuration=" --configuration-file $conf_file "
  fi

  if [[ "$CONFIG_KEY" != "" ]]; then
    configuration=" $configuration --configuration-key $CONFIG_KEY "
  fi

  local topology_file="$config_dir/topology$i.yaml"
  #local kademlia_file="$config_dir/kademlia$i.yaml"
  local updater_file="$config_dir/updater$i.sh"

  echo -n " --db-path $run_dir/node-db$i $rts_opts $reb $keys_args"

  ekg_server="127.0.0.1:$((8000+i))"
  export statsd_server="127.0.0.1:$((8125+i))"

  # A sloppy test but it'll do for now.
  local topology_first_six_bytes
  topology_first_six_bytes=$(head -c 6 "$topology_file" )
  if [[ "$topology_first_six_bytes" != "wallet" ]]; then
    # 'wallet' may not bind, otherwise throws error "BehindNAT topology is used, no bind address is expected"
    echo -n " --listen 127.0.0.1:$(get_port "$i")"
  fi
  if [[ "$configuration" != "" ]]; then
    echo -n " $configuration "
  fi
  declare time_lord
  declare stats
  echo -n " $(logs "$log_dir" "node$i.log") ${time_lord} ${stats}"
  echo -n " $web "
  echo -n " $report_server "
  echo -n " $wallet_args "
  echo -n " --system-start $system_start"
  echo -n " --metrics +RTS -T -RTS"
  echo -n " --ekg-server $ekg_server"
  #echo -n " --statsd-server $statsd_server"
  echo -n " --node-id node$i"
  echo -n " --topology $topology_file"
  #echo -n " --kademlia $kademlia_file"
  # Use the policies option if you want to change enqueue/dequeue/failure
  # policies without re-compiling. See example files
  #   scripts/policies/policy_core.yaml
  #   scripts/policies/policy_relay.yaml
  #echo -n " --policies $config_dir/policy$i.yaml"
  echo -n "  --update-latest-path $updater_file"
  echo -n "  --update-with-package"
  echo -n "  --update-server 'http://127.0.0.1:10228/'"
  echo ''
  sleep 0.8
}

function bench_cmd {
  local i=$1
  local system_start=$2
  local time=$3
  local conc=$4
  local delay=$5
  ensure_run

  echo -n "$(find_binary cardano-auxx)"
  # This assumes that the n-1 node is the relay
  echo -n " --peer 127.0.0.1:$(get_port $((i-1)))"
  echo -n " $(logs "" node_auxx.log)"
  echo -n " --system-start $system_start"
  # First arg is the number of transactions with input from genesis block and
  # the second is the total number of transactions to be sent.
  echo -n " cmd --commands \"send-to-all-genesis $time $time $conc $delay ./tps-sent.csv\""
  echo -n " --configuration-key smallbench "
  echo -n " --rebuild-db "

  echo ''
}



function has_nix {
  command -v nix-shell 2> /dev/null
  return $?
}

function stack_build {
  if [[ $(has_nix) == 0 ]]; then
    echo "Building with nix-shell"
    stack --nix build --test --no-run-tests --bench --no-run-benchmarks --fast
  else
    echo "Building normally"
    stack build --test --no-run-tests --bench --no-run-benchmarks --fast
  fi
}
