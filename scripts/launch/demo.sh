#!/usr/bin/env bash
set -eo xtrace

declare WALLET_FLUSH

# Make sure we're in a tmux session.
if ! [ -n "$TMUX" ]; then
  echo "You must run this script from the tmux session!"
  exit 1
fi

# Check the `--nix` option (must be first option)
nix=""
if [[ $1 == "--nix" ]]; then
  shift
  nix="--nix"
fi

# Make sure we're using proper version of tmux.
tmux_actual_version=$(tmux -V | awk '{print $2}')
# All tmux versions contain two numbers only.
tmux_proper_versions=("2.3" "2.4" "2.5" "2.6" "2.7" "master")
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
# shellcheck disable=SC1090
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
  pwd
  gen_kademlia_topology $n
fi
run_dir=$config_dir

# Stats are not mandatory either
stats=$4
export stats

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
    system_start=$(($(date +%s) + 15))
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
  echo "$args" | sed 's/+RTS.*-RTS//g' | tr " " "\\n" | grep -vE '^$' | while read -r l; do
    echo -n " $prefix $l"
  done
}

function make_yaml_list {
  local items="$1"
  echo "$items" | sed 's/+RTS.*-RTS//g' | tr " " "\\n" | grep -vE '^$' | while read -r l; do
    echo "- \"$l\""
  done
}

i=0
while [[ $i -lt $panesCnt ]]; do
  im=$((i%4))
  ir=$((i/4))

  if [[ $im == 0 ]]; then
    # TODO (akegalj): use `tmux new-session -s seassion-name` instead
    tmux new-window -n "demo-$(date +%H%M%S)-$ir"
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
  x509GenTool=$(find_binary cardano-x509-certificates ${nix})
  if [[ $WALLET_TEST != "" ]] && [[ $i == $((n-1)) ]]; then
      if [[ $WALLET_CONFIG != "" ]]; then
          conf_file=$WALLET_CONFIG
      fi

      if [ ! -d ${config_dir}/tls-files ]; then
        mkdir -p ${config_dir}/tls-files
        $x509GenTool \
          --server-out-dir ${config_dir}/tls-files \
          --clients-out-dir ${config_dir}/tls-files \
          --configuration-key  default \
          --configuration-file lib/configuration.yaml
      fi

      wallet_args=" --tlscert $config_dir/tls-files/server.crt --tlskey $config_dir/tls-files/server.key --tlsca $config_dir/tls-files/ca.crt $wallet_flush" # --wallet-rebuild-db'
      wallet_args="$WALLET_EXTRA_ARGS $wallet_args --wallet-address 127.0.0.1:8090"
      exec_name="$WALLET_EXE_NAME"
      if [[ $WALLET_DEBUG != "" ]]; then
          wallet_args="$wallet_args --wallet-debug"
      fi
  fi
  if [[ $WALLET_TEST != "" ]] && [[ $i == $((n-2)) ]]; then
      if [[ $WALLET_CONFIG != "" ]]; then
          conf_file=$WALLET_CONFIG
      fi
      wallet_args=" --tlscert $base/../tls-files/server.crt --tlskey $base/../tls-files/server.key --tlsca $base/../tls-files/ca.crt $wallet_flush" # --wallet-rebuild-db'
      wallet_args="$WALLET_EXTRA_ARGS --new-wallet $wallet_args --wallet-address 127.0.0.1:8091"
      exec_name="$WALLET_EXE_NAME"
      if [[ $WALLET_DEBUG != "" ]]; then
          wallet_args="$wallet_args --wallet-debug"
      fi
  fi
  if [[ $i -lt $n ]]; then
    node_args="$(node_cmd $i "$wallet_args" "$system_start" "$config_dir" "$conf_file" "$run_dir" "$run_dir/logs")"
    node_=$(find_binary $exec_name $nix)
    if [[ $WALLET_TEST != "" ]] && [[ $i -ge $((n-2)) ]]; then
        updater_file="$config_dir/updater$i.sh"
        launcher_=$(find_binary cardano-launcher $nix)

        ensure_run $run_dir

        # shellcheck disable=SC2154
        full_node_args="$node_args $reb $no_ntp $keys_args $rts_opts"
        if [[ $WALLET_CLIENT_AUTH_DISABLE != "" ]]; then
            # shellcheck disable=SC2154
            full_node_args="$full_node_args --no-client-auth"
        fi

        CONFIG_PATH="$run_dir/launcher-config-$i.yaml"

        {
            echo "nodePath: $node_"
            echo "nodeArgs:"
            echo -e "$(make_yaml_list "$full_node_args")"

            echo "nodeDbPath: $run_dir/node-db$i"

            if [[ "$UI" != "" ]]; then
              echo "walletLogging: true"
              echo "walletPath: $UI"
              echo "walletArgs:"
              echo -e "$(make_yaml_list "$UI_ARGS")"
            fi
            echo "x509ToolPath: $x509GenTool"

            echo "tlsPath: ${config_dir}/tls-files"
            echo "updaterPath: /usr/bin/env"
            echo "updaterArgs: [bash]"
            echo "updateArchive: $updater_file"
            echo "nodeTimeoutSec: 5"
            echo "configuration:"
            echo "  filePath: lib/configuration.yaml"
            echo "  key: default"
            # The following is required by `withConfigurations`
            # (specifically, `withCoreConfigurations`). See
            # Pos.Core.Configuration for more details.
            echo "  systemStart: 0"
        } > $CONFIG_PATH

        launcher_args="-c $CONFIG_PATH"
        tmux send-keys "$launcher_ $launcher_args" C-m
    else
        echo "$node_ $node_args"
        tmux send-keys "$node_ $node_args" C-m
    fi
  else
    # Number of transactions to send per-thread: 300
    # Concurrency (number of threads sending transactions); $CONC
    # Delay between sends on each thread: 500 milliseconds
    tmux send-keys "sleep 40s && $(bench_cmd "$i" "$system_start" "$NUM_TXS" "$CONC" 500 neighbours)" C-m
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
