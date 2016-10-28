
function find_binary {
	echo $(stack path --local-install-root)/bin/$1
}

function ensure_logs {
	mkdir -p logs

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
}
