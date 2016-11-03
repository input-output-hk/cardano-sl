#!/usr/bin/env bash

base=$(dirname "$0")
. "$base"/../common.sh
ensure_logs

function supporter_dht_conf {
  echo -n " --peer '127.0.0.1:2000/ABOtPlQMv123_4wzfgjAzvsT2LE='"
}

node_cmd $1 "$TIME_LORD" "supporter_dht_conf" | /usr/bin/env bash
