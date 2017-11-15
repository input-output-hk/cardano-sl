#!/usr/bin/env bash
set -eo xtrace

# Make sure we're in a tmux session.
if ! [ -n "$TMUX" ]; then
  echo "You must run this script from the tmux session!"
  exit 1
fi

# Make sure we're using proper version of tmux.
tmux_actual_version=$(tmux -V | awk '{print $2}')
# All tmux versions contain two numbers only.
tmux_proper_versions=("2.3" "2.4" "2.5" "2.6" "master")
checker=""
for version in "${tmux_proper_versions[@]}"; do
    if [ "${version}" == "${tmux_actual_version}" ]; then
        checker="${version}"; break
    fi
done
# If checker is still empty - we're using wrong tmux version!
if [ "${checker}" == "" ]; then
    echo "Please upgrade tmux to the version ${tmux_proper_versions[0]} or higher."
    exit 1
fi

base=$(dirname "$0")
source "$base"/../common-functions.sh

# If stack-work doesn't exist use function
if [[ ! -d "$base/../../.stack-work" ]]; then
    stack_build
    # TODO: Maybe wanna have an option to rebuild?
fi

# Define the default amount of nodes to run
DEFAULT_NODES_N=4

# Detect node number or use default
n=$1
if [[ "$n" == "" ]]; then
  n=$DEFAULT_NODES_N
fi

# RICH_NODES specifies how many nodes should be core nodes, i.e., have non-negligible stake in the rich_poor_distr
if [[ "$RICH_NODES" == "" ]]; then
  RICH_NODES=$n
fi

config_dir=$2

if [[ $config_dir == "" ]]; then
  config_dir="./run"
  if [[ ! -d $config_dir ]]; then
    mkdir $config_dir
  fi
  echo $(pwd)
  gen_kademlia_topology $n
fi


# Stats are not mandatory either
stats=$4

panesCnt=$n

if [[ "$CONC" != "" ]]; then
  panesCnt=$((n+1))
fi

if [[ "$NUM_TXS" == "" ]]; then
  NUM_TXS=3000
fi

wallet_flush=""
# Argument for testing wallet db flushing
if [[ "$WALLET_FLUSH" != "" ]]; then
  wallet_flush="--flush-wallet-db"
fi

# System start time in seconds (time since epoch).
# An extra second is added so that the nodes have extra time to start up
# and start processing the first slot.
if [ -z "$system_start" ]
  then
    system_start=$((`date +%s` + 15))
fi

# This enables to select different wallet executables, since we have a
# migration from the old to the new wallet.
if [ -z "$WALLET_EXE_NAME" ]
  then
    WALLET_EXE_NAME="cardano-node"
fi


echo "Using system start time "$system_start

echo "Number of panes: $panesCnt"

function prefix_args {
  local args="$1"
  local prefix="$2"
  echo "$args" | sed 's/+RTS.*-RTS//g' | tr " " "\n" | grep -vE '^$' | while read l; do
    echo -n " $prefix $l"
  done
}

i=0
while [[ $i -lt $panesCnt ]]; do
  im=$((i%4))
  ir=$((i/4))

  if [[ $im == 0 ]]; then
    tmux new-window -n "demo-"`date +%H%M%S`-"$ir"
    tmux split-window -h
    tmux split-window -v
    tmux select-pane -t 0
    tmux split-window -v
  fi

  echo "Launching node $i in tab $im of window $ir"
  tmux select-pane -t $im

  conf_file=$CONFIG
  wallet_args=''
  exec_name='cardano-node-simple'
  if [[ $WALLET_TEST != "" ]] && [[ $i == $((n-1)) ]]; then
      if [[ $WALLET_CONFIG != "" ]]; then
          conf_file=$WALLET_CONFIG
      fi
      wallet_args=" --tlscert $base/../tls-files/server.crt --tlskey $base/../tls-files/server.key --tlsca $base/../tls-files/ca.crt $wallet_flush" # --wallet-rebuild-db'
      wallet_args="$wallet_args --wallet-address 127.0.0.1:8090"
      exec_name='cardano-node'
      if [[ $WALLET_DEBUG != "" ]]; then
          wallet_args="$wallet_args --wallet-debug"
      fi
  fi
  if [[ $i -lt $n ]]; then
    node_args="$(node_cmd $i "$stats" "$wallet_args" "$system_start" "$config_dir" "$conf_file")"
    node_=$(find_binary $exec_name)
    if [[ $WALLET_TEST != "" ]] && [[ $i == $((n-1)) ]]; then
        updater_file="$config_dir/updater$i.sh"
        launcher_=$(find_binary cardano-launcher)
        launcher_args="--node $node_ "
        if [[ "$UI" != "" ]]; then
          ui_args_="$(prefix_args "$UI_ARGS" "-w")"
          launcher_args=" $launcher_args --wlogging --wallet $UI $ui_args_"
        fi
        node_args_="$(prefix_args "$node_args" "-n")"
        launcher_args=" $launcher_args --updater /usr/bin/env -u bash --update-archive $updater_file"
        launcher_args=" $launcher_args --node-timeout 5 $node_args_ --system-start 0  --db-path $run_dir/node-db$i $rts_opts $reb $no_ntp $keys_args"
        tmux send-keys "$launcher_ $launcher_args" C-m
    else
        echo "$node_ $node_args"
        tmux send-keys "$node_ $node_args" C-m
    fi
  else
    # Number of transactions to send per-thread: 300
    # Concurrency (number of threads sending transactions); $CONC
    # Delay between sends on each thread: 500 milliseconds
    tmux send-keys "sleep 40s && $(bench_cmd $i "$system_start" $NUM_TXS $CONC 500 neighbours)" C-m
  fi
  i=$((i+1))
done

# Experimental:
# This would hang the script and wait for Ctrl+C
# trap "$base/kill-demo.sh; exit" INT
# while true; do
#     # nothing: just wait until the user presses Ctrl+C
#     sleep 9000
# done
